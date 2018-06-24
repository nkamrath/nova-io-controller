----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    03:29:25 03/12/2018 
-- Design Name: 
-- Module Name:    neopixel_controller - Behavioral 
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

entity neopixel_controller is
	generic 
	(
		gCLOCK_PERIOD_NS			: natural := 20;  -- 50 Mhz clock is 2 ns period
		gT0H_TIMING_NS				: natural := 300;
		gT1H_TIMING_NS				: natural := 600;
		gT0L_TIMING_NS				: natural := 900;
		gT1L_TIMING_NS				: natural := 600;
		gTRST_TIMING_NS			: natural := 80000;
		gCOLOR_BIT_WIDTH			: natural := 2;
		gCOLORS_PER_PIXEL			: natural := 3;
		gINTENSITY_BIT_WIDTH		: natural := 8;
		gADDRESS_BIT_WIDTH		: natural := 6;
		gNUM_PIXELS					: natural := 12
	);
	port ( 
		CLK 					: in  STD_LOGIC;
		nRESET 				: in  STD_LOGIC;
		WRITE_EN 			: in  STD_LOGIC;
		COLOR 				: in  STD_LOGIC_VECTOR ((gCOLOR_BIT_WIDTH-1) downto 0);
		INTENSITY 			: in  STD_LOGIC_VECTOR ((gINTENSITY_BIT_WIDTH-1) downto 0);
		ADDRESS 				: in  STD_LOGIC_VECTOR ((gADDRESS_BIT_WIDTH-1) downto 0);
		NEOPIXEL_CONTROL 	: out  STD_LOGIC
	);
end neopixel_controller;

architecture Behavioral of neopixel_controller is

	constant cBITS_PER_PIXEL	: natural := (gINTENSITY_BIT_WIDTH * gCOLORS_PER_PIXEL);

	TYPE output_state_t IS (t0h, t1h, t0l, t1l, trst);
	type pixel_map_t is ARRAY(natural range 0 to (gNUM_PIXELS - 1)) of std_logic_vector((cBITS_PER_PIXEL - 1) downto 0);
	
	constant cT0H_CLOCK_DIV		: natural := (gT0H_TIMING_NS / gCLOCK_PERIOD_NS);
	constant cT1H_CLOCK_DIV		: natural := (gT1H_TIMING_NS / gCLOCK_PERIOD_NS);
	constant cT0L_CLOCK_DIV		: natural := (gT0L_TIMING_NS / gCLOCK_PERIOD_NS);
	constant cT1L_CLOCK_DIV		: natural := (gT1L_TIMING_NS / gCLOCK_PERIOD_NS);
	constant cTRST_CLOCK_DIV	: natural := (gTRST_TIMING_NS / gCLOCK_PERIOD_NS);
	
	constant cCOLOR_RED			: std_logic_vector((gCOLOR_BIT_WIDTH-1) downto 0) := "00";
	constant cCOLOR_GREEN		: std_logic_vector((gCOLOR_BIT_WIDTH-1) downto 0) := "01";
	constant cCOLOR_BLUE			: std_logic_vector((gCOLOR_BIT_WIDTH-1) downto 0) := "10";
	
	signal pixel_map				: pixel_map_t;
	
	signal pixel_counter			: natural range 0 to (gNUM_PIXELS-1);
	signal bit_counter			: natural range 0 to (cBITS_PER_PIXEL-1);
	signal clock_counter			: natural range 0 to (cTRST_CLOCK_DIV-1);
	signal current_output_state : output_state_t;
	
begin

	update_pixel_map : process(CLK)
	begin
		if(rising_edge(CLK)) then
			if(nRESET = '0') then
				pixel_map <= (others => "00000000000000000000000000000000");
				--pixel_map(0) <= "00000000000000000000011100000000";--(others => '0'));
				--pixel_map(1) <= "00000000000000000000011100000000";--(others => '0'));
			else
				if(WRITE_EN = '1') then
					if(COLOR = cCOLOR_RED) then
						pixel_map(to_integer(unsigned(ADDRESS)))(23 downto 16) <= INTENSITY;
					elsif(COLOR = cCOLOR_GREEN) then
						pixel_map(to_integer(unsigned(ADDRESS)))(31 downto 24) <= INTENSITY;
					elsif(COLOR = cCOLOR_BLUE) then
						pixel_map(to_integer(unsigned(ADDRESS)))(15 downto 8) <= INTENSITY;
					end if;
				end if;
			end if;
		end if;	
	end process;

	update_clock_counter : process(CLK)
	begin
		if(rising_edge(CLK)) then
			if(nRESET = '0') then
				clock_counter <= 0;
				bit_counter <= (cBITS_PER_PIXEL-1);
				pixel_counter <= 0;
				current_output_state <= trst;
			else
				--handle trst state
				if(current_output_state = trst and clock_counter = (cTRST_CLOCK_DIV-1)) then
					--start at first bit of first pixel
					clock_counter <= 0;
					bit_counter <= (cBITS_PER_PIXEL-1);
					pixel_counter <= 0;
					if(pixel_map(0)((cBITS_PER_PIXEL-1)) = '0') then
						current_output_state <= t0h;
					else
						current_output_state <= t1h;
					end if;
				--handle low part of 0
				elsif(current_output_state = t0h and clock_counter >= (cT0H_CLOCK_DIV-1)) then
					current_output_state <= t0l;
					clock_counter <= 0;
				--handle low part of 1
				elsif(current_output_state = t1h and clock_counter >= (cT1H_CLOCK_DIV-1)) then
					current_output_state <= t1l;
					clock_counter <= 0;
				--if we are done with a bit
				elsif((current_output_state = t0l and clock_counter >= (cT0L_CLOCK_DIV-1)) or (current_output_state = t1l and clock_counter >= (cT1L_CLOCK_DIV-1))) then
					clock_counter <= 0;
					--if we are moving pixels
					if(bit_counter = 0) then
						bit_counter <= (cBITS_PER_PIXEL-1);
						--if we are moving pixels
						if(pixel_counter = (gNUM_PIXELS-1)) then
							pixel_counter <= 0;
							current_output_state <= trst;
							--if(pixel_map(0)((cBITS_PER_PIXEL-1)) = '0') then
							--	current_output_state <= t0h;
							--else
							--	current_output_state <= t1h;
							--end if;
						else
							pixel_counter <= pixel_counter +1;
							if(pixel_map(pixel_counter+1)((cBITS_PER_PIXEL-1)) = '0') then
								current_output_state <= t0h;
							else
								current_output_state <= t1h;
							end if;
						end if;
					else
						bit_counter <= bit_counter - 1;
						if(pixel_map(pixel_counter)(bit_counter-1) = '0') then
							current_output_state <= t0h;
						else
							current_output_state <= t1h;
						end if;
					end if;
				else
					clock_counter <= clock_counter + 1;
				end if;
			end if;
		end if;
	end process;

	update_control_pin : process(CLK)
	begin
		if(rising_edge(CLK)) then
			if(nRESET = '0') then
					NEOPIXEL_CONTROL <= '0';
			else
				if(current_output_state = t0h or current_output_state = t1h) then
					NEOPIXEL_CONTROL <= '1';
				else
					NEOPIXEL_CONTROL <= '0';
				end if;
			end if;
		end if;
	end process update_control_pin;

end Behavioral;

