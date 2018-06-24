----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    20:13:26 12/29/2017 
-- Design Name: 
-- Module Name:    top - Behavioral 
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
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

library unisim;
use unisim.vcomponents.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity top is
    Port ( 
				CLK 					: in  STD_LOGIC;
				nRESET 				: in  STD_LOGIC;
				UART_RX_PINS 		: in  STD_LOGIC_VECTOR(1 downto 0);
				UART_TX_PINS 		: out STD_LOGIC_VECTOR(1 downto 0);
				LED					: out STD_LOGIC_VECTOR(7 downto 0);
				
				DIGITAL_IN 			: in std_logic_vector(9 downto 0);
				DIGITAL_OUT 		: out std_logic_vector(9 downto 0);
				
				NEOPIXEL_CONTROL  : out std_logic
			);
end top;

architecture Behavioral of top is

	component io_controller
		generic
		(
			gFIFO_SIZE		: natural := 100;
			gNUM_INPUTS		: natural := 10;
			gNUM_OUTPUTS   : natural := 10;
			gNUM_REGISTERS : natural := 10;
			gNUM_UARTS		: natural := 2
		);
		Port 
		( 
			CLK 					: in  STD_LOGIC;
			nRESET 				: in  STD_LOGIC;
			UART_RX_PINS 		: in STD_LOGIC_VECTOR((gNUM_UARTS-1) downto 0);
			UART_TX_PINS		: out STD_LOGIC_VECTOR((gNUM_UARTS-1) downto 0);
			INPUTS   			: in  STD_LOGIC_VECTOR((gNUM_INPUTS-1) downto 0);
			OUTPUTS  			: out STD_LOGIC_VECTOR((gNUM_OUTPUTS-1) downto 0);
			NEOPIXEL_CONTROL  : out  STD_LOGIC
		);
	end component;
	
	signal reset						: std_logic;
	signal clock_counter				: natural range 0 to 50000000 := 0;
	signal led_state					: std_logic := '0';
	
	signal high_count					: natural range 0 to 127;
	
begin

	reset <= not nRESET;
	
	io_controller_inst : io_controller port map (
		CLK => CLK,
		nRESET => nRESET,
		UART_RX_PINS	=> UART_RX_PINS,
		UART_TX_PINS	=> UART_TX_PINS,
		INPUTS => DIGITAL_IN,
		OUTPUTS => DIGITAL_OUT,
		NEOPIXEL_CONTROL => NEOPIXEL_CONTROL
	);
	
	update_led : process(CLK)
	begin
		if(rising_edge(CLK)) then
			if(clock_counter = (50000000-1)) then
				clock_counter <= 0;
				LED(0) <= led_state;
				led_state <= not led_state;
			else
				clock_counter <= clock_counter + 1;
			end if;
			LED(7 downto 1) <= std_logic_vector(to_unsigned(high_count, 7));
		end if;
	end process update_led;

end Behavioral;

