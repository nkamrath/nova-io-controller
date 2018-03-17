----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    10:28:24 02/27/2016 
-- Design Name: 
-- Module Name:    uart_controller - Behavioral 
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
use ieee.numeric_std.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity uart_controller is
generic
(
  gFIFO_WIDTH          : natural;  -- data width of one more than actual data.  1 in lsb signfies continuation of packet, 0 means last byte of a packet
  gFIFO_LENGTH         : natural;
  gMAX_PAYLOAD_LENGTH  : natural
);
port
(
  CLK               : in  std_logic;
  RESET             : in  std_logic;
  TX_DATA           : in  std_logic_vector((gFIFO_WIDTH - 1) downto 0);
  TX_DATA_VALID     : in  std_logic;
  RX_DATA_READ      : in  std_logic;
  PACKET_REMOVED    : in  std_logic;
  UART_RX_PIN       : in  std_logic;
  PACKET_READY      : out std_logic;
  UART_TX_PIN       : out std_logic;
  RX_DATA           : out std_logic_vector((gFIFO_WIDTH - 1) downto 0);
  RX_DATA_VALID     : out std_logic;
  LED               : out std_logic_vector(7 downto 0)
);
end uart_controller;

architecture Behavioral of uart_controller is

  component fifo
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
  
  component uart_interface
  port
  (
    CLK                 : in  std_logic;
    RESET               : in  std_logic;
    NEW_DATA_AVAILABLE  : in  std_logic;
    NEW_DATA            : in  std_logic_vector(7 downto 0);
    UART_RX_PIN         : in  std_logic;
    VALID               : out std_logic;
	NEW_DATA_TAKEN      : out std_logic;
    UART_DATA_OUT       : out std_logic_vector(7 downto 0);
    UART_TX_PIN         : out std_logic
  );
  end component;
  
  type state_t is (STATE_TO_ADDRESS, STATE_FROM_ADDRESS,
                   STATE_LEN_SUM, STATE_PAYLOAD);
  
  signal state                         : state_t;
  signal uart_interface_data_in        : std_logic_vector(7 downto 0);
  signal uart_interface_data_available : std_logic;
  signal uart_interface_data_taken     : std_logic;
  signal uart_interface_data_taken2    : std_logic;
  signal uart_interface_data_taken_rising : std_logic;
  signal uart_interface_valid          : std_logic;
  signal uart_interface_data_out       : std_logic_vector(7 downto 0);
  
  signal payload_counter               : natural range 0 to (gMAX_PAYLOAD_LENGTH);
  signal payload_length                : natural range 0 to (gMAX_PAYLOAD_LENGTH);
  signal rx_fifo_packet_counter        : natural range 0 to (gFIFO_LENGTH - 1);
  signal new_packet_added              : std_logic;
  signal new_packet_added2             : std_logic;
  signal new_packet_added_rising       : std_logic;
  --counts packets in the rx fifo

begin

uart_interface_inst : uart_interface port map
(
  CLK => CLK,
  RESET => RESET,
  NEW_DATA_AVAILABLE => uart_interface_data_available,
  NEW_DATA => uart_interface_data_in,
  UART_RX_PIN => UART_RX_PIN,
  VALID => uart_interface_valid,
  NEW_DATA_TAKEN => uart_interface_data_taken,
  UART_DATA_OUT => uart_interface_data_out,
  UART_TX_PIN => UART_TX_PIN
);

rx_fifo : fifo port map
(
  CLK => CLK,
  RESET => RESET,
  DATA_IN => uart_interface_data_out,
  ADD_DATA => uart_interface_valid,
  REMOVE_DATA => RX_DATA_READ,
  DATA_OUT => RX_DATA,
  HAS_DATA => RX_DATA_VALID
  --SIZE =>
);

tx_fifo : fifo port map
(
  CLK => CLK,
  RESET => RESET,
  DATA_IN => TX_DATA,
  ADD_DATA => TX_DATA_VALID,
  REMOVE_DATA => uart_interface_data_taken,
  DATA_OUT => uart_interface_data_in,
  HAS_DATA => uart_interface_data_available
  --SIZE =>
);

update_state : process(CLK)
begin
  if(rising_edge(CLK)) then
    if(RESET = '1') then
	  state <= STATE_TO_ADDRESS;
	  payload_length <= 0;
	  payload_counter <= 0;
	  LED <= (others => '0');
	elsif(uart_interface_valid = '1') then
	  case state is
	    --when STATE_IDLE =>
		  --state <= STATE_TO_ADDRESS;
		  --LED <= uart_interface_data_out;
		when STATE_TO_ADDRESS =>
		  state <= STATE_FROM_ADDRESS;
		  LED <= uart_interface_data_out;
		when STATE_FROM_ADDRESS =>
		  state <= STATE_LEN_SUM;
		when STATE_LEN_SUM =>
		  state <= STATE_PAYLOAD;
		  payload_counter <= 0;
		  payload_length <= (to_integer(unsigned(uart_interface_data_out(7 downto 6))) + 1);
		  --if(to_integer(unsigned(uart_interface_data_out(7 downto 6))) = 1) then
		  --  state <= STATE_IDLE;
		  --else
		  --  state <= STATE_PAYLOAD;
		  --end if;
		when STATE_PAYLOAD =>
		  if(payload_counter >= payload_length - 1) then -- payload length field is 3 bits where 00 = 1
		    --state <= STATE_IDLE;
			 state <= STATE_TO_ADDRESS;
		  else
		   payload_counter <= payload_counter + 1;
		  end if;
		end case;
		
	end if;
  end if;
end process update_state;

update_packet_added : process(CLK)
begin
  if(rising_edge(CLK)) then
    if(RESET = '1') then
	  new_packet_added <= '0';
	  new_packet_added2 <= '0';
	else
	  new_packet_added2 <= new_packet_added;
	  if(state = STATE_PAYLOAD and payload_counter >= payload_length - 1 and uart_interface_valid = '1') then
	    new_packet_added <= '1';  --this needs to be delayed a clock!!!!!
	  else
	    new_packet_added <= '0';
	  end if;
	  
	  if(new_packet_added = '1' and new_packet_added2 = '0') then
	    new_packet_added_rising <= '1';
	  else
	    new_packet_added_rising <= '0';
	  end if;
	  
	end if;
  end if;
end process update_packet_added;

update_packet_counter : process(CLK)
begin
  if(rising_edge(CLK)) then
    if(RESET = '1') then
	  rx_fifo_packet_counter <= 0;
	else
	  if(PACKET_REMOVED = '1' and new_packet_added_rising = '0' and rx_fifo_packet_counter > 0) then
	    rx_fifo_packet_counter <= rx_fifo_packet_counter - 1;
	  elsif(PACKET_REMOVED = '0' and new_packet_added_rising = '1') then
	    rx_fifo_packet_counter <= rx_fifo_packet_counter + 1;
	  end if;
	end if;
  end if;
end process update_packet_counter;

update_packet_ready : process(CLK)
begin
  if(rising_edge(CLK)) then
    if(RESET = '1') then
	  PACKET_READY <= '0';
	else
	  if(rx_fifo_packet_counter > 0) then
	    PACKET_READY <= '1';
	  else
	    PACKET_READY <= '0';
	  end if;
	end if;
  end if;
end process update_packet_ready;

--update_uart_interface_data_taken_rising : process(CLK)
--begin
--  if(rising_edge(CLK)) then
--    if(RESET = '1') then
--	   uart_interface_data_taken2 <= '0';
--	   uart_interface_data_taken_rising <= '0';
--	 else
--	   uart_interface_data_taken2 <= uart_interface_data_taken;
--		if(uart_interface_data_taken = '1' and uart_interface_data_taken2 = '0') then
--		  uart_interface_data_taken_rising <= '1';
--		else
--		  uart_interface_data_taken_rising <= '0';
--		end if;
--	 end if;
--  end if;
--end process update_uart_interface_data_taken_rising;

--update_led : process(CLK)
--begin
--  if(rising_edge(CLK)) then
--    if(RESET = '1') then
--	  LED <= (others => '0');
--	else
--	  if(uart_interface_valid = '1') then
--	    LED <= uart_interface_data_out;
--	  end if;
--	end if;
--  end if;
--end process update_led;

--LED <= (others => '1');

end Behavioral;