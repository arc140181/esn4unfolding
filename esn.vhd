----------------------------------------------------------------------------------
-- Company: 
-- Engineer: Alberto Regadío 
-- 
-- Create Date:    15:08:15 05/06/2022 
-- Design Name: 
-- Module Name:    esn - Behavioral 
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
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;
USE ieee.math_real.all;
library res_lib;
use res_lib.res_pkg.all;

entity esn is
	 generic ( UNIT : integer := UNIT; -- Number of neurons
	           FP : integer := FP);  -- Fixed point
    port (  x : in STD_LOGIC_VECTOR(FP-1 downto 0);
	       xen : in STD_LOGIC;
           d : in  STD_LOGIC_VECTOR(FP-1 downto 0);
           den : in  STD_LOGIC;
           clk : in  STD_LOGIC;
		   rst : in STD_LOGIC;
           y : out  STD_LOGIC_VECTOR(FP downto 0);
		   yen : out STD_LOGIC);
end esn;

architecture Behavioral of esn is
	 component reservoir
	 generic ( UNIT : integer := UNIT; -- Number of neurons
	           FP : integer := FP);  -- Fixed point
    port ( x : in reg_t(0 to 0);
           clk : in  STD_LOGIC;
           rst : in  STD_LOGIC;
           y_out : out reg_t(0 to UNIT-1));
	 end component;

	COMPONENT dense
	generic ( W : integer := FP;
	   DEC : integer := 2**7;
	   ALPHA : integer := 2**10;
	   Nx : integer := UNIT);
	PORT(
		x : in reg_t(0 to UNIT-1);
	   xen : in STD_LOGIC;
      d : in  STD_LOGIC_VECTOR(W-1 downto 0);
      den : in  STD_LOGIC;
      clk : in  STD_LOGIC;
		rst : in STD_LOGIC;
      y : out  STD_LOGIC_VECTOR(W downto 0);
		yen : out STD_LOGIC
	);
	END COMPONENT;

	signal x_i : reg_t(0 to 0);
	signal h_i : reg_t(0 to UNIT-1);

begin

	x_p: process(clk)
	begin
		if rising_edge(clk) then
			if rst = '1' then
				x_i(0) <= 0;
			elsif xen = '1' then
				x_i(0) <= conv_integer(x);
			end if;
		end if;
	end process;

	reservoir1: reservoir
	GENERIC MAP(
		UNIT => UNIT,
	   FP => FP)
   PORT MAP (
		x => x_i,
      clk => clk,
      rst => rst,
      y_out => h_i
	);
	
	dense1: dense
	GENERIC MAP(
		W => FP,
	   DEC => 2**7,
	   ALPHA => 2**11,
	   Nx => UNIT
	)
	PORT MAP(
		x => h_i,
		xen => '1',
		d => d,
		den => den,
		clk => clk,
		rst => rst,
		y => y,
		yen => yen
	);

end Behavioral;

