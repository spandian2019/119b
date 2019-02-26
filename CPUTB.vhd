----------------------------------------------------------------------------
--
--  Test Bench for CPU
--
--  This is a test bench for the AVR CPU entity. It checks the program
--  address bus, data data bus for both read/write operations, data address
--  bus, and data read and write signals. The program data is specified
--  in the PROG_MEMORY entity.
--
--
--  Revision History:
--  02/20/2019 Sophia Liu Initial revision
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.opcodes.all;
use work.constants.all;
use work.TBConstants.all;

entity CPUTB is
end CPUTB;

architecture TB_ARCHITECTURE of CPUTB is

    -- test component declarations
    -- CPU to test
    component  AVR_CPU  is
        port (
            ProgDB  :  in     std_logic_vector(15 downto 0);   -- program memory data bus
            Reset   :  in     std_logic;                       -- reset signal (active low)
            INT0    :  in     std_logic;                       -- interrupt signal (active low)
            INT1    :  in     std_logic;                       -- interrupt signal (active low)
            clock   :  in     std_logic;                       -- system clock
            ProgAB  :  out    std_logic_vector(15 downto 0);   -- program memory address bus
            DataAB  :  out    std_logic_vector(15 downto 0);   -- data memory address bus
            DataWr  :  out    std_logic;                       -- data memory write enable (active low)
            DataRd  :  out    std_logic;                       -- data memory read enable (active low)
            DataDB  :  inout  std_logic_vector(7 downto 0)     -- data memory data bus
        );
    end  component;
    -- test ROM
    component  PROG_MEMORY  is
        port (
            ProgAB  :  in   std_logic_vector(15 downto 0);  -- program address bus
            Reset   :  in   std_logic;                      -- system reset
            ProgDB  :  out  std_logic_vector(15 downto 0)   -- program data bus
        );
    end  component;
    -- test RAM
    component  DATA_MEMORY  is
        port (
            RE      : in     std_logic;             	-- read enable (active low)
            WE      : in     std_logic;		        -- write enable (active low)
            DataAB  : in     std_logic_vector(15 downto 0); -- memory address bus
            DataDB  : inout  std_logic_vector(7 downto 0)   -- memory data bus
        );
    end  component;

    -- Signal used to stop clock signal generators
    signal  END_SIM  :  BOOLEAN := FALSE;

    -- Stimulus signals - signals mapped to the input and output ports of tested entity
    signal ProgDB  :  std_logic_vector(15 downto 0);    -- second word of instruction
    signal Reset   :  std_logic;                        -- system reset signal (active low)
    signal INT0    :  std_logic;                       -- interrupt signal (active low)
    signal INT1    :  std_logic;                       -- interrupt signal (active low)
    signal clock   :  std_logic;                       -- system clock
    signal ProgAB  :  std_logic_vector(15 downto 0);   -- program memory address bus
    signal DataAB  :  std_logic_vector(15 downto 0);    -- data address bus
    signal DataDB  :  std_logic_vector(7 downto 0);     -- data data bus
    signal DataRd  :  std_logic;                        -- data read (active low)
    signal DataWr  :  std_logic;                        -- data write (active low)

    signal Clk     : std_logic; -- system clock

    begin
	   -- test components
        UUT: AVR_CPU
        port map(
            ProgDB  => ProgDB,
            Reset   => Reset,
            INT0    => INT0,
            INT1    => INT1,
            clock   => Clk,
            ProgAB  => ProgAB,
            DataAB  => DataAB,
            DataDB  => DataDB,
            DataRd  => DataRd,
            DataWr  => DataWr
        );

        UUTP: PROG_MEMORY
        port map(
            ProgAB => ProgAB,
            Reset => Reset,
            ProgDB => ProgDB
        );

        UUTD: DATA_MEMORY
        port map(
            RE => DataRd,
            WE => DataWr,
            DataAB => DataAB,
            DataDB => DataDB
        );

        -- generate the stimulus and test the design
        TB: process
            variable i : integer;
            variable j : integer;
        begin


        	-- initially everything is 0, have not started
            Reset <= '0'; -- begin with reset
            INT0 <= '1'; -- disable interrupts
            INT1 <= '1';
        	wait for CLK_PERIOD*5.5; -- wait for a bit

            wait for CLK_PERIOD*0.2; -- offset for clock edge
            Reset <= '1'; -- de-assert reset, program should begin from start

			-- check with test vectors every clock
			for i in TEST_SIZE downto 0 loop
                -- on falling edge
                wait for CLK_PERIOD/2;
                -- check prog AB
                assert (to_integer(unsigned(ProgABTest(i))) = to_integer(unsigned(ProgAB)))
                    report  "ProgAB failure at clock number " & integer'image(TEST_SIZE-i)
                    severity  ERROR;

                -- check data AB (for ld, st)
                assert (std_match(DataABTest(i), DataAB))
                    report  "DataAB failure at clock number " & integer'image(TEST_SIZE-i)
                    severity  ERROR;

                -- check data DB (for ld)
                assert (std_match(DataDBRdTest(i), DataDB))
                    report  "DataDBRd failure at clock number " & integer'image(TEST_SIZE-i)
                    severity  ERROR;

                -- check data DB (for st)
                assert (std_match(DataDBWrTest(i), DataDB))
                        report  "DataDBWr failure at clock number " & integer'image(TEST_SIZE-i)
                        severity  ERROR;

                -- check rd/wr (check for glitches)
                assert (std_match(DataRdTest(i), DataRd))
                    report  "DataRd DataDBRd failure at test number " & integer'image(TEST_SIZE-i)
                    severity  ERROR;
                -- check write not asserted
                assert (std_match(DataWrTest(i), DataWr))
                    report  "DataWr DataDBRd at test number " & integer'image(TEST_SIZE-i)
                    severity  ERROR;

                wait for CLK_PERIOD/2; -- wait for rest of clock

			end loop;
            wait for CLK_PERIOD*50;
            END_SIM <= TRUE;        -- end of stimulus events
            wait;                   -- wait for simulation to end
        end process;

        -- not writing to dataDB, hi-z
        dataDB <= (others => 'Z');

        -- process for generating system clock
        CLOCK_CLK : process
        begin
            -- this process generates a CLK_PERIOD ns 50% duty cycle clock
            -- stop the clock when the end of the simulation is reached
            if END_SIM = FALSE then
                CLK <= '0';
                wait for CLK_PERIOD/2;
            else
                wait;
            end if;

            if END_SIM = FALSE then
                CLK <= '1';
                wait for CLK_PERIOD/2;
            else
                wait;
            end if;
        end process;

end TB_ARCHITECTURE;

--configuration TESTBENCH_FOR_CPU of CPUTB is
--    for TB_ARCHITECTURE
--		for UUT : AVR_CPU
--            use entity work.AVR_CPU;
--        end for;
--        for UUTP : PROG_MEMORY
--            use entity work.PROG_MEMORY;
--        end for;
--        for UUTD : DATA_MEMORY
--            use entity work.DATA_MEMORY;
--        end for;
--    end for;
--end TESTBENCH_FOR_CPU;
