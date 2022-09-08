----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    14:37:06 05/06/2022 
-- Design Name: 
-- Module Name:    dense - Behavioral 
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

library res_lib;
use res_lib.res_pkg.all;

entity dense is
	 generic ( W : integer := FP;
	           DEC : integer := 2**7;
	           ALPHA : integer := 2**10; --2*10
			   Nx : integer := UNIT);

--    port ( x : in  STD_LOGIC_VECTOR(W-1 downto 0);
--	       xen : in STD_LOGIC;
--           d : in  STD_LOGIC_VECTOR(W-1 downto 0);
--           den : in  STD_LOGIC;
--           clk : in  STD_LOGIC;
--		   rst : in STD_LOGIC;
--           y : out  STD_LOGIC_VECTOR(W downto 0);
--		   yen : out STD_LOGIC);

    port ( x : in reg_t(0 to UNIT-1);
	       xen : in STD_LOGIC;
           d : in  STD_LOGIC_VECTOR(W-1 downto 0);
           den : in  STD_LOGIC;
           clk : in  STD_LOGIC;
		   rst : in STD_LOGIC;
           y : out  STD_LOGIC_VECTOR(W downto 0);
		   yen : out STD_LOGIC);
end dense;

architecture Behavioral of dense is

	function initw( min_val : integer; max_val : integer) return reg_t is
	    variable w : reg_t(0 to UNIT-1);
		variable seed1, seed2 : positive;
		variable rand : real;
	begin
		for i in 0 to UNIT-1 loop
			uniform(seed1, seed2, rand);
			w(i) := integer((rand * real(max_val - min_val)) + real(min_val));
		end loop;
		return w;
	end initw;

	--type x_signed is array (0 to Nx-1) of integer range (-(2**(W - 1))) to (2**(W - 1) - 1);

	-- Coefficients
	signal h : reg_t(0 to UNIT-1);
	
	-- Block delays
	signal x_z : reg_t(0 to UNIT-1);
	
	-- Output signal
	signal y_i : integer;
	
	-- Desired output value
	signal ds: integer range (-(2**(W - 1))) to (2**(W - 1) - 1);
	
	-- Internal buffered signals
	signal rst_i : std_logic;
	signal xen_i : std_logic;
	signal den_z: std_logic;
	
	signal err : integer;

begin
	BUFG_rst_inst : BUFG
    port map (
      O => rst_i,     -- Clock buffer output
      I => rst        -- Clock buffer input
    );
	
	BUFG_xen_inst : BUFG
    port map (
      O => xen_i,     -- Clock buffer output
      I => xen        -- Clock buffer input
    );
	
	yen <= (not rst_i) and (not den_z) and xen_i;
	
	den_p: process(clk)
	begin
		if rising_edge(clk) then
			den_z <= den;
		end if;
	end process;
	
	d_p: process(rst_i, clk)
	begin
		if rst_i = '1' then
			ds <= 0;
		elsif rising_edge(clk) then
			ds <= conv_integer(d);
		end if;
	end process;
		
--	x_p: process(clk)
--	begin
--		if rising_edge(clk) then
--			if rst_i = '1' then
--				for i in 0 to Nx-1 loop
--					x_z(i) <= 0;
--				end loop;
--			elsif xen_i = '1' then
--				x_z(0) <= conv_integer(x);
--				for i in 1 to Nx-1 loop
--					x_z(i) <= x_z(i-1);
--				end loop;
--			end if;
--		end if;
--	end process;

	process(xen_i, x)
	begin
		if xen_i = '1' then
			x_z <= x;
		end if;
	end process;
	
	y_p: process(clk)
		variable acc : integer := 0;
		variable error: integer;
	begin
		if rising_edge(clk) then
			if rst_i = '1' then
				y_i <= 0;
				h <= initw(-100,100);
				--for i in 0 to Nx-1 loop
                --    h(i) <= DEC;
                --end loop;
                err <= 0;
			else
			    acc := 0;
			    error := 0;
				for i in 0 to Nx-1 loop
					acc := acc + (h(i) * x_z(i)) / (Nx * DEC);
				end loop;
				
				--if acc < 0 then
				--    acc := 0;
				--end if;
				
				y_i <= acc;
				
				if den_z = '1' then
					error := ds - acc;

					   for i in 0 to Nx-1 loop
						  h(i) <= h(i) + (error * x_z(i)) / (ALPHA * DEC);
					   end loop;

				end if;
				err <= error;
			end if;
		end if;
	end process;

    process(y_i)
    begin
        if y_i < 0 then
            y <= (others => '0');
        else
	        y <= conv_std_logic_vector(y_i, W+1);
	    end if;
	end process;

end Behavioral;