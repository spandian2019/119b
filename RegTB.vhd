----------------------------------------------------------------------------
--
--  Test Bench for Registers
--
--  This is a test bench for the SerialDivider entity. The test bench
--  thoroughly tests the entity by exercising it and checking the outputs
--  through the use of an array of test values (TestVector). The test bench
--  entity is called ALUTB.
--
--
--  Revision History:
--  01/30/2019 Sophia Liu Initial revision
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

use work.opcodes.all;

use work.constants.all;

entity REGTB is
    -- timing constants for testing
    constant CLK_PERIOD : time := 20 ns;
    constant TEST_SIZE: natural := 12;
end REGTB;

architecture TB_ARCHITECTURE of REGTB is

    -- test component declaration
    component REG_TEST is
        port(
            IR       :  in  opcode_word;                        -- Instruction Register
            RegIn    :  in  std_logic_vector(7 downto 0);       -- input register bus
            clock    :  in  std_logic;                          -- system clock
            RegAOut  :  out std_logic_vector(7 downto 0);       -- register bus A out
            RegBOut  :  out std_logic_vector(7 downto 0)        -- register bus B out
        );
    end component;

    -- Signal used to stop clock signal generators
    signal  END_SIM  :  BOOLEAN := FALSE;

    -- Stimulus signals - signals mapped to the input and inout ports of tested entity
    signal IR       : opcode_word;
    signal clock    : std_logic; -- system clock
    signal RegIn    : std_logic_vector(7 downto 0);
    signal RegAOut  : std_logic_vector(7 downto 0);
    signal RegBOut  : std_logic_vector(7 downto 0);

    type IRVector    is array (integer range <>) of opcode_word;
    type VectorCases is array (integer range <>) of std_logic_vector(7 downto 0);

    begin
        UUT: REG_TEST
        port map(
            --Clk     => Clk,
            IR      => IR,
            RegIn   => RegIn,
            clock   => clock,
            RegAOut => RegAOut,
            RegBOut => RegBOut
        );

        -- generate the stimulus and test the design
        TB: process
        variable  i  :  integer := 0;

        variable IRTest : IRVector(TEST_SIZE downto 0);
        variable casesIn : VectorCases(TEST_SIZE downto 0);
        variable casesA : VectorCases(TEST_SIZE downto 0);
        variable casesB : VectorCases(TEST_SIZE downto 0);

        begin

            -- initially everything is X, have not started
            IR      <= "0000000000000000";
            RegIn   <= "00000000";
            RegAOut <= "00000000";
            RegBOut <= "00000000";
            wait for CLK_PERIOD*5;

            IRTest  := ("0111000000000000", -- R16 ANDI $00
                        "0111000000010000", -- R17 ANDI $00
                        "0111000011100000", -- R30 ANDI $00
                        "0111000011110000", -- R31 ANDI $00
                        "0110000000011111", -- R17 ORI  $0F
                        "0000111100000001", -- R16 <- R16 + R17
                        "1001010100001010", -- R16 <- R16 - 1
                        "0010101100000001", -- R16 <- R16 OR R17
                        "1001011100110001", -- R31:R30 <- R31:R30 + $01FF
                        "1001011100110001", -- R31:R30 <- R31:R30 + $01FF
                        "1001010000001000", -- SReg(0) <- '1'
                        "1111101100000000", -- T <- R16(0)
                        "0001111111100001");-- R30 <- R30 + R17

            casesIn := (X"00", X"00", X"00", X"00", X"0F", X"0F",
                        X"0E", X"0F", X"FF", X"01", X"FF", X"0F", X"10");
            casesA  := (X"00", X"00", X"00", X"00", X"0F", X"0F",
                        X"0E", X"0F", X"FF", X"01", X"FF", X"0F", X"10");
            casesB  := (X"00", X"00", X"00", X"00", X"0F", X"0F",
                        X"01", X"0F", X"FF", X"01", X"FF", X"0F", X"0F");

           IR <= IRTest(i);

           wait for CLK_PERIOD*0.50;
           RegIn <= casesIn(i);
           wait for CLK_PERIOD*0.50;

           -- check result
           assert (std_match(casesA(i), RegAOut))
                report  "A reg failure"
                severity  ERROR;
           -- check pre-masked sreg
           assert (std_match(casesB(i), RegBOut))
                report  "B reg failure"
                severity  ERROR;

            for i in TEST_SIZE to 0 loop
               IR <= IRTest(i);

               wait for CLK_PERIOD*0.75;
               RegIn <= casesIn(i);
               wait for CLK_PERIOD*0.25;

               -- check result
               assert (std_match(casesA(i), RegAOut))
                    report  "A reg failure"
                    severity  ERROR;
               -- check pre-masked sreg
               assert (std_match(casesB(i), RegBOut))
                    report  "B reg failure"
                    severity  ERROR;
            end loop;

            END_SIM <= TRUE;        -- end of stimulus events
            wait;                   -- wait for simulation to end
        end process;

        -- process for generating system clock
        CLOCK_CLK : process
        begin
            -- this process generates a 20 ns 50% duty cycle clock
            -- stop the clock when the end of the simulation is reached
            if END_SIM = FALSE then
                clock <= '0';
                wait for CLK_PERIOD/2;
            else
                wait;
            end if;

            if END_SIM = FALSE then
                clock <= '1';
                wait for CLK_PERIOD/2;
            else
                wait;
            end if;
        end process;

end TB_ARCHITECTURE;

configuration TESTBENCH_FOR_REG of REGTB is
    for TB_ARCHITECTURE
          for UUT : REG_TEST
            use entity work.REG_TEST;
        end for;
    end for;
end TESTBENCH_FOR_REG;