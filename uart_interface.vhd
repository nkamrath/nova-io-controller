----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    01:54:27 12/30/2017 
-- Design Name: 
-- Module Name:    uart_interface - Behavioral 
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity uart_interface is
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

end uart_interface;

architecture Behavioral of uart_interface is

	component uart
	GENERIC(
		clk_freq		:	INTEGER		:= 50_000_000;	--frequency of system clock in Hertz
		baud_rate	:	INTEGER		:= 19_200;		--data link baud rate in bits/second
		os_rate		:	INTEGER		:= 16;			--oversampling rate to find center of receive bits (in samples per baud period)
		d_width		:	INTEGER		:= 8; 			--data bus width
		parity		:	INTEGER		:= 0;				--0 for no parity, 1 for parity
		parity_eo	:	STD_LOGIC	:= '0');			--'0' for even, '1' for odd parity
	port
	(
		clk		:	IN		STD_LOGIC;										--system clock
		reset_n	:	IN		STD_LOGIC;										--ascynchronous reset
		tx_ena	:	IN		STD_LOGIC;										--initiate transmission
		tx_data	:	IN		STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  --data to transmit
		rx			:	IN		STD_LOGIC;										--receive pin
		rx_busy	:	OUT	STD_LOGIC;										--data reception in progress
		rx_error	:	OUT	STD_LOGIC;										--start, parity, or stop bit error detected
		rx_data	:	OUT	STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);	--data received
		tx_busy	:	OUT	STD_LOGIC;  									--transmission in progress
		tx_taken	:  OUT   STD_LOGIC;										--goes high when tx data has been taken
		tx			:	OUT	STD_LOGIC);										--transmit pin
	end component;
	
	component fifo
	generic
	(
	  gFIFO_WIDTH      : natural := 8;
	  gFIFO_LENGTH     : natural := gFIFO_SIZE
	  
	);
	port
	(
	  CLK          : in  std_logic;
	  RESET        : in  std_logic;
	  DATA_IN      : in  std_logic_vector((gFIFO_WIDTH - 1) downto 0);
	  ADD_DATA     : in  std_logic;
	  REMOVE_DATA  : in  std_logic;
	  DATA_OUT     : out std_logic_vector((gFIFO_WIDTH - 1) downto 0);
	  HAS_DATA     : out std_logic;
	  SIZE         : out natural range 0 to (gFIFO_LENGTH - 1)
	);
	end component;
	
	
	signal uart_rx_data				: std_logic_vector(7 downto 0);
	signal uart_tx_data				: std_logic_vector(7 downto 0);
	signal uart_tx_enable			: std_logic;
	signal uart_tx_busy				: std_logic;
	signal uart_rx_busy				: std_logic;
	signal uart_rx_busy_z			: std_logic;
	signal uart_tx_taken				: std_logic;
	signal uart_rx_data_ready		: std_logic;
	signal reset 						: std_logic;

begin

	reset <= not nRESET;

	uart_inst : uart port map
	(
		CLK => CLK,
		reset_n => nRESET,
		tx_ena => uart_tx_enable,
		tx_data => uart_tx_data,
		rx => UART_RX_PIN,
		rx_busy => uart_rx_busy,
		rx_data => uart_rx_data,
		tx => UART_TX_PIN,
		tx_busy => uart_tx_busy,
		tx_taken => uart_tx_taken
	);
	
	tx_fifo : fifo port map
	(
		CLK          => CLK,
		RESET        => reset,
		DATA_IN      => TX_DATA,
		ADD_DATA     => TX_DATA_ADD,
		REMOVE_DATA  => uart_tx_taken,
		DATA_OUT     => uart_tx_data,
		HAS_DATA     => uart_tx_enable,
		SIZE         => TX_FIFO_SIZE
	);
	
	rx_fifo : fifo port map
	(
		CLK          => CLK,
		RESET        => reset,
		DATA_IN      => uart_rx_data,
		ADD_DATA     => uart_rx_data_ready,
		REMOVE_DATA  => RX_DATA_TAKEN,
		DATA_OUT     => RX_DATA,
		--HAS_DATA     => uart_tx_enable,
		SIZE         => RX_FIFO_SIZE
	);
	
	update_uart_rx_data: process(CLK)
	begin
		if(rising_edge(CLK)) then
			if(nRESET = '0') then
				uart_rx_busy_z <= '0';
				uart_rx_data_ready <= '0';
			else
				uart_rx_busy_z <= uart_rx_busy;
				if(uart_rx_busy = '0' and uart_rx_busy_z = '1') then
					uart_rx_data_ready <= '1';
				else
					uart_rx_data_ready <= '0';
				end if;
			end if;
		end if;
	end process;

end Behavioral;

