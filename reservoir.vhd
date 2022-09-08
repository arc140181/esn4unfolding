library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;
USE ieee.math_real.all;

library res_lib;
use res_lib.res_pkg.all;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity reservoir is
	 generic ( UNIT : integer := UNIT; -- Number of neurons
	           FP : integer := FP);  -- Fixed point
    port ( x : in reg_t(0 to 0);
           clk : in  STD_LOGIC;
           rst : in  STD_LOGIC;
           y_out : out reg_t(0 to UNIT-1));
end reservoir;

architecture Behavioral of reservoir is
    signal x_z : reg_t(0 to UNIT-1);
	signal y : reg_t(0 to UNIT-1);
	signal z : reg_t(0 to UNIT-1);
	
	constant CONNECTION : real := 0.6;

	function  matmul(constant a : mat_t; b: reg_t ) return reg_t is
		variable i, j : integer;
		variable prod : reg_t(0 to UNIT-1);
	begin
	    for i in 0 to UNIT-1 loop
	       prod(i) := 0;
	    end loop;
		for i in 0 to UNIT-1 loop
			for j in 0 to UNIT-1 loop
			    if a(i)(j) /= 0 then
				    prod(i) := prod(i) + (a(i)(j) * b(j)) / 2**(FP);
				end if;
			end loop;
		end loop;
		return prod;
	end matmul;
	
	function initw( min_val : integer; max_val : integer) return mat_t is
	   variable w : mat_t;
		variable seed1, seed2 : positive;
		variable rand : real;
	begin
		for i in 0 to UNIT-1 loop
			for j in 0 to UNIT-1 loop
				uniform(seed1, seed2, rand);
				if rand < CONNECTION then
					uniform(seed1, seed2, rand);
					w(i)(j) := integer((rand * real(max_val - min_val)) + real(min_val));
				else
					w(i)(j) := 0;
				end if;
			end loop;
		end loop;
		return w;
	end initw;
	
	constant w : mat_t := initw(-2**(FP-2), 2**(FP-2));
	
begin

    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                x_z(0) <= 0;
            else
                x_z(0) <= x(0);
            end if;
        end if;
    end process;

	process(clk)
	begin
		if falling_edge(clk) then
			z(0) <= x(0);
			z(1) <= x(0) - x_z(0);
			z(2) <= x_z(0);
			z(3 to UNIT-1) <= y(3 to UNIT-1);
		end if;
	end process;
			
	process(clk)
	begin
		if rising_edge(clk) then
			if rst = '1' then
				for i in 0 to UNIT-1 loop
					y(i) <= 0;
				end loop;
			else
				y <= matmul(w, z);
			end if;
		end if;
	end process;
	
	y_out <= y;

end Behavioral;
