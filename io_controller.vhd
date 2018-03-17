----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    16:31:01 12/30/2017 
-- Design Name: 
-- Module Name:    io_controller - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity io_controller is
	generic
	(
		gFIFO_SIZE			: natural := 20;
		gNUM_INPUTS			: natural := 2;
		gNUM_OUTPUTS   	: natural := 2;
		gADDRESS_WIDTH 	: natural := 8;
		gREGISTER_WIDTH 	: natural := 32;
		gNUM_REGISTERS 	: natural := 2;
		gNUM_UARTS			: natural := 2
	);
	Port 
	( 
		CLK 					: in  STD_LOGIC;
		nRESET 				: in  STD_LOGIC;
		UART_RX_PINS 		: in STD_LOGIC_VECTOR((gNUM_UARTS-1) downto 0);
		UART_TX_PINS		: out STD_LOGIC_VECTOR((gNUM_UARTS-1) downto 0);
		INPUTS   			: in  STD_LOGIC_VECTOR((gNUM_INPUTS-1) downto 0);
		OUTPUTS  			: out STD_LOGIC_VECTOR((gNUM_OUTPUTS-1) downto 0)
	);
end io_controller;

architecture Behavioral of io_controller is
	
	component uart_interface
		generic
		(
			gFIFO_SIZE			: natural := 100
		);
		port
		(
			CLK					: in  std_logic;
			nRESET				: in  std_logic;
			UART_RX_PIN			: in 	std_logic;
			TX_DATA_ADD			: in  std_logic;
			TX_DATA				: in  std_logic_vector(7 downto 0);
			RX_DATA_TAKEN		: in  std_logic;
			RX_FIFO_SIZE		: out natural range 0 to (gFIFO_SIZE-1);
			TX_FIFO_SIZE		: out natural range 0 to (gFIFO_SIZE-1);
			RX_DATA				: out std_logic_vector(7 downto 0);
			UART_TX_PIN			: out std_logic
		);
	end component;
	
	
	TYPE rx_machine IS(idle, marker, wait_marker, parse1, wait1, parse2, wait2, write_data, wait_tx, wait3);		--tranmit state machine data type
	TYPE tx_machine IS(idle, tx1, tx2);															--tranmit state machine data type
	
	-- dio_rw_inv is dio with inverted output (output of 1 indicates a digital output signal of 0
	TYPE register_type_t IS (dio_r, dio_rw, dio_rw_inv, pwm, pwm_inv, data);
	TYPE register_type_array_t IS ARRAY(natural range 0 to (gNUM_REGISTERS - 1)) of register_type_t;
	
	TYPE slv_8_t IS ARRAY(natural range 0 to (gNUM_UARTS-1)) of std_logic_vector(7 downto 0);
	TYPE fifo_size_array_t IS ARRAY(natural range 0 to (gNUM_UARTS-1)) of natural range 0 to (gFIFO_SIZE-1);
	
	type register_map_t is ARRAY(natural range 0 to (gNUM_REGISTERS - 1)) of std_logic_vector((gREGISTER_WIDTH - 1) downto 0);
	
	constant register_types	   : register_type_array_t := (
		dio_rw_inv, pwm_inv); --dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw,
--		dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw,
--		dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw,
--		dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw, dio_rw,
--		data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data,
--		data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data,
--		data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data,
--		data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data
--	);

	constant UART_MARKER_BYTE			 : std_logic_vector(7 downto 0) := x"55";
	
	constant DIO_DIGITAL_WRITE_INDEX  : natural := 0;
	constant DIO_CURRENT_INPUT_INDEX  : natural := 1;
	constant DIO_PREVIOUS_INPUT_INDEX : natural := 2;
	constant DIO_CURRENT_OUTPUT_INDEX : natural := 3;
	
	constant PWM_ENABLE_INDEX			 : natural := 12;
	constant PWM_WRITE_MAX_INDEX		 : natural := 12;
	constant PWM_CURRENT_OUTPUT_STATE_INDEX : natural := 13;
	constant PWM_LAST_PIN_INPUT_INDEX		 : natural := 14;
	
	constant INPUT_CLOCK_FREQUENCY_HZ : natural := 50_000_000;
	constant HZ_PWM_DIVISOR				 : natural := 500_000;
	signal pwm_counter					 : natural range 0 to (HZ_PWM_DIVISOR-1) := 0;
	signal pwm_output_1z					 : std_logic := '0';
	signal pwm_output_2z					 : std_logic := '0';
	
	signal register_map       : register_map_t;
	
	signal rx_state				: rx_machine;
	signal tx_state				: tx_machine;
	
	signal uart_rx_data				: slv_8_t;
	signal uart_tx_data				: slv_8_t;
	signal uart_tx_ready				: std_logic_vector((gNUM_UARTS-1) downto 0);
	signal uart_rx_taken				: std_logic_vector((gNUM_UARTS-1) downto 0);
	
	signal current_uart				: natural range 0 to (gNUM_UARTS-1) := 0;
	signal tx_uart						: natural range 0 to (gNUM_UARTS-1) := 0;
	
	signal uart_tx_fifo_size		: fifo_size_array_t;
	signal uart_rx_fifo_size		: fifo_size_array_t;
	
	signal rx_address					: std_logic_vector((gADDRESS_WIDTH-1) downto 0);
	signal rx_data						: std_logic_vector((gREGISTER_WIDTH-1) downto 0);
	signal start_tx					: std_logic;
	
	signal tx_address					: std_logic_vector((gADDRESS_WIDTH-1) downto 0);
	signal tx_data						: std_logic_vector((gREGISTER_WIDTH-1) downto 0);
	signal tx_done						: std_logic;
	
begin

	uart_interface0 : uart_interface port map
	(
		CLK => CLK,
		nRESET => nRESET,
		UART_RX_PIN => UART_RX_PINS(0),
		TX_DATA_ADD => uart_tx_ready(0),
		TX_DATA => uart_tx_data(0),
		RX_DATA_TAKEN => uart_rx_taken(0),
		RX_FIFO_SIZE => uart_rx_fifo_size(0),
		TX_FIFO_SIZE => uart_tx_fifo_size(0),
		RX_DATA => uart_rx_data(0),
		UART_TX_PIN => UART_TX_PINS(0)
	);
	
	uart_interface1 : uart_interface port map
	(
		CLK => CLK,
		nRESET => nRESET,
		UART_RX_PIN => UART_RX_PINS(1),
		TX_DATA_ADD => uart_tx_ready(1),
		TX_DATA => uart_tx_data(1),
		RX_DATA_TAKEN => uart_rx_taken(1),
		RX_FIFO_SIZE => uart_rx_fifo_size(1),
		TX_FIFO_SIZE => uart_tx_fifo_size(1),
		RX_DATA => uart_rx_data(1),
		UART_TX_PIN => UART_TX_PINS(1)
	);
	
	switch_interfaces : process(CLK)
	begin
		if(rising_edge(CLK)) then
			if(nRESET = '0') then
				current_uart <= 0;
			else
				if(rx_state = idle and uart_rx_fifo_size(current_uart) = 0) then
					if(current_uart = (gNUM_UARTS-1)) then
						current_uart <= 0;
					else
						current_uart <= current_uart + 1;
					end if;
				end if;
			end if;
		end if;
	end process;
	
	handle_rx_machine : process(CLK)
	begin
		if(rising_edge(CLK)) then
			if(nRESET = '0') then
				rx_state <= idle;
				start_tx <= '0';
				rx_address <= (others => '0');
				rx_data <= (others => '0');
				uart_rx_taken <= (others => '0');
				tx_uart <= 0;
			else
				case rx_state is
					--idle, waiting for the marker byte
					when idle =>
						if(uart_rx_fifo_size(current_uart) > 0) then
							uart_rx_taken(current_uart) <= '1';
							if(uart_rx_data(current_uart) = UART_MARKER_BYTE) then
								rx_state <= wait_marker;
							end if;
						else
							uart_rx_taken(current_uart) <= '0';
						end if;
					--we got the marker, now wait for first byte of read or second byte of a read
					when marker =>
						if(uart_rx_fifo_size(current_uart) > 0 and uart_rx_data(current_uart)(7) = '0') then
							--this is a read
							start_tx <= '1';
							tx_uart <= current_uart;
							tx_address <= uart_rx_data(current_uart);
							rx_state <= wait_tx;
							uart_rx_taken(current_uart) <= '1';
						elsif(uart_rx_fifo_size(current_uart) > 2 and uart_rx_data(current_uart)(7) = '1') then
							--this is a write
							rx_state <= wait1;
							rx_address <= uart_rx_data(current_uart);
							uart_rx_taken(current_uart) <= '1';
							start_tx <= '0';
						else
							uart_rx_taken(current_uart) <= '0';
							start_tx <= '0';
							rx_state <= marker;
						end if;
					--read the first byte of rx data payload (not the address)
					when parse1 =>
						start_tx <= '0';
						rx_data(7 downto 0) <= uart_rx_data(current_uart);
						uart_rx_taken(current_uart) <= '1';
						rx_state <= wait2;
					--read the second byte of the rx data payload (not the address)
					when parse2 =>
						rx_data(15 downto 8) <= uart_rx_data(current_uart);
						uart_rx_taken(current_uart) <= '1';
						rx_state <= write_data;
					--data now being written to the register map
					when write_data =>
						uart_rx_taken(current_uart) <= '0';
						rx_state <= wait3;
					--wait states between all the parsing and processing states
					--these wait states keep fifo reads and processing all in sync
					when wait_marker =>
						rx_state <= marker;
						uart_rx_taken(current_uart) <= '0';
					when wait1 =>
						rx_state <= parse1;
						uart_rx_taken(current_uart) <= '0';
					when wait2 =>
						rx_state <= parse2;
						uart_rx_taken(current_uart) <= '0';
					when wait3 =>
						uart_rx_taken(current_uart) <= '0';
						rx_state <= idle;
						start_tx <= '0';
					when wait_tx =>
						uart_rx_taken(current_uart) <= '0';
						start_tx <= '0';
						rx_state <= wait3;
				end case;
			end if;
		end if;
	end process;
	
	handle_tx_machine : process(CLK)
	begin
		if(rising_edge(CLK)) then
			if(nRESET = '0') then
				tx_state <= idle;
				uart_tx_ready <= (others => '0');
				uart_tx_data <= (others => (others => '0'));
			else
				case tx_state is
					when idle =>
						if(start_tx = '1') then
							if(to_integer(unsigned(tx_address(7 downto 0))) > 63) then
								tx_data <= "0000000000000000000000000000000" & INPUTS(64-to_integer(unsigned(tx_address(7 downto 0))));
							else
								tx_data <= register_map(to_integer(unsigned(tx_address(7 downto 0))));
							end if;
							uart_tx_data(tx_uart) <= tx_address;
							uart_tx_ready(tx_uart) <= '1';
							tx_state <= tx1;
						else
							uart_tx_ready(tx_uart) <= '0';
							tx_state <= idle;
						end if;
						
					when tx1 =>
						uart_tx_data(tx_uart) <= tx_data(7 downto 0);
						uart_tx_ready(tx_uart) <= '1';
						tx_state <= tx2;
					
					when tx2 =>
						uart_tx_data(tx_uart) <= tx_data(15 downto 8);
						uart_tx_ready(tx_uart) <= '1';
						tx_state <= idle;
				end case;
			end if;
		end if;
	end process;
	
	update_register_map : process(CLK)
	begin
		if(rising_edge(CLK)) then
			if(nRESET = '0') then
				register_map <= (others => (others => '0'));
			else
			
			   --update register map digital write value
				if(rx_state = write_data) then
					if(register_types(to_integer(unsigned(rx_address(6 downto 0)))) = dio_rw or register_types(to_integer(unsigned(rx_address(6 downto 0)))) = dio_rw_inv) then
						register_map(to_integer(unsigned(rx_address(6 downto 0))))(DIO_DIGITAL_WRITE_INDEX) <= rx_data(0);
						register_map(to_integer(unsigned(rx_address(6 downto 0))))(DIO_CURRENT_OUTPUT_INDEX) <= rx_data(0);
					elsif(register_types(to_integer(unsigned(rx_address(6 downto 0)))) = pwm or register_types(to_integer(unsigned(rx_address(6 downto 0)))) = pwm_inv) then
						--if we are enabling, reset the pwm counter and set the output high
						if(rx_data(PWM_ENABLE_INDEX) = '1') then
							register_map(to_integer(unsigned(rx_address(6 downto 0))))(25 downto 16) <= register_map(to_integer(unsigned(rx_address(6 downto 0))))(9 downto 0);
							register_map(to_integer(unsigned(rx_address(6 downto 0))))(PWM_CURRENT_OUTPUT_STATE_INDEX) <= '1';
						else
							register_map(to_integer(unsigned(rx_address(6 downto 0))))(25 downto 16) <= register_map(to_integer(unsigned(rx_address(6 downto 0))))(9 downto 0);
							register_map(to_integer(unsigned(rx_address(6 downto 0))))(PWM_CURRENT_OUTPUT_STATE_INDEX) <= '0';
						end if;
						register_map(to_integer(unsigned(rx_address(6 downto 0))))(PWM_WRITE_MAX_INDEX downto 0) <= rx_data(PWM_WRITE_MAX_INDEX downto 0);  --only certain bit writes to pwm registers
					end if;
				end if;
				
				--Update register map from inputs and outputs
				for I in 0 to (gNUM_REGISTERS-1) loop
					if(register_types(I) = dio_rw or register_types(I) = dio_rw_inv) then
						register_map(I)(DIO_CURRENT_INPUT_INDEX) <= inputs(I);
						register_map(I)(DIO_PREVIOUS_INPUT_INDEX) <= register_map(I)(DIO_CURRENT_INPUT_INDEX);
						if(register_map(I)(DIO_CURRENT_INPUT_INDEX) /= register_map(I)(DIO_PREVIOUS_INPUT_INDEX)) then
							register_map(I)(DIO_CURRENT_OUTPUT_INDEX) <= register_map(I)(DIO_CURRENT_INPUT_INDEX);
						end if;
					--if it is a pwm and the period is greater than 0, update it.  PWM with period of zero is basically
					--equivalent to a dio and is only toggled by writing the enable bit on/off
					elsif(register_types(I) = pwm or register_types(I) = pwm_inv) then
						--if the pwm is enabled we need to process it
						register_map(I)(PWM_LAST_PIN_INPUT_INDEX) <= inputs(I);
						if(register_map(I)(PWM_LAST_PIN_INPUT_INDEX) /= inputs(I)) then
							register_map(I)(PWM_ENABLE_INDEX) <= inputs(I);
							register_map(I)(PWM_CURRENT_OUTPUT_STATE_INDEX) <= inputs(I);
						end if;
						if(register_map(I)(PWM_ENABLE_INDEX) = '1') then
							--rising edge of pwm output means we decrement the internal count register
							if(pwm_output_1z = '1' and pwm_output_2z = '0' and unsigned(register_map(I)(9 downto 0)) > 0) then
								if(unsigned(register_map(I)(25 downto 16)) = 0) then
									--flip the state and reset
									register_map(I)(25 downto 16) <= register_map(I)(9 downto 0);
									register_map(I)(PWM_CURRENT_OUTPUT_STATE_INDEX) <= not register_map(I)(PWM_CURRENT_OUTPUT_STATE_INDEX);
								else
									--otherwise decrement one from the internal counter state
									register_map(I)(25 downto 16) <= std_logic_vector(unsigned(register_map(I)(25 downto 16))-1);
								end if;
							end if;
						end if;
					end if;
				end loop;
				
			end if;
		end if;
	end process;
	
	handle_led : process(CLK)
	begin
		if(rising_edge(CLK)) then
			if(nRESET = '0') then
				for I in 0 to (gNUM_OUTPUTS-1) loop
				    if(register_types(I) = dio_rw) then
						outputs(I) <= '0';
					 elsif(register_types(I) = dio_rw_inv) then
						outputs(I) <= '1';
					 end if;
				end loop;
			else
				--outputs(led_address) <= register_map(led_address)(3);
				for I in 0 to (gNUM_OUTPUTS-1) loop
				    if(register_types(I) = dio_rw) then
						outputs(I) <= register_map(I)(DIO_CURRENT_OUTPUT_INDEX);
					 elsif(register_types(I) = dio_rw_inv) then
						outputs(I) <= not register_map(I)(DIO_CURRENT_OUTPUT_INDEX);
					 elsif(register_types(I) = pwm) then
						outputs(I) <= register_map(I)(PWM_CURRENT_OUTPUT_STATE_INDEX);
					 elsif(register_types(I) = pwm_inv) then
						outputs(I) <= not register_map(I)(PWM_CURRENT_OUTPUT_STATE_INDEX);
					 end if;
				end loop;
			end if;
		end if;
	end process;
	
	gen_hz_pwm: process(CLK)
	begin
		if(rising_edge(CLK)) then
			if(nRESET = '0') then
				pwm_counter <= 0;
				pwm_output_1z <= '0';
				pwm_output_2z <= '0';
			else
				if(pwm_counter = (HZ_PWM_DIVISOR-1)) then
					pwm_counter <= 0;
					pwm_output_1z <= '1';
				else
					pwm_counter <= pwm_counter + 1;
					pwm_output_1z <= '0';
				end if;
				pwm_output_2z <= pwm_output_1z;
			end if;
		end if;
	end process;

end Behavioral;

