----------------------------------------------------------------------------
--
--  Test Bench for Registers
--
--  This is a test bench for the Regteste entity. The test bench
--  thoroughly tests the entity by exercising it and checking the outputs
--  through the use of an array of test values (TestVector). The test bench
--  entity is called REGTB.
--
--
--  Revision History:
--  02/01/2019   Sundar Pandian  Initial Revision
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

use work.opcodes.all;

use work.constants.all;

entity REGTB is
    -- timing constant for testing
    constant CLK_PERIOD : time := 20 ns;
    -- Test vector size
    constant TEST_SIZE: integer := 12;
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
    signal clock    : std_logic;
    signal RegIn    : std_logic_vector(7 downto 0);
    signal RegAOut  : std_logic_vector(7 downto 0);
    signal RegBOut  : std_logic_vector(7 downto 0);

    type IRVector    is array (integer range <>) of opcode_word;
    type VectorCases is array (integer range <>) of std_logic_vector(7 downto 0);

    begin
        UUT: REG_TEST
        port map(
            IR      => IR,
            RegIn   => RegIn,
            clock   => clock,
            RegAOut => RegAOut,
            RegBOut => RegBOut
        );

        -- generate the stimulus and test the design
        TB: process
        variable  i  :  integer;

        variable IRTest : IRVector(TEST_SIZE downto 0);
        variable casesIn : VectorCases(TEST_SIZE downto 0);
        variable casesA : VectorCases(TEST_SIZE downto 0);
        variable casesB : VectorCases(TEST_SIZE downto 0);

        begin

            -- initially everything is 0, have not started
            IR      <= "0000000000000000";
            RegIn   <= "00000000";
            wait for CLK_PERIOD*5.5;

            IRTest  := ("0111000000000000", -- R16 ANDI $00
                        "0111000000010000", -- R17 ANDI $00
                        "0111000011100000", -- R30 ANDI $00
                        "0111000011110000", -- R31 ANDI $00
                        "0110000000011111", -- R17 ORI  $0F
                        "0000111100000001", -- R16 <- R16 + R17
                        "1001010100001010", -- R16 <- R16 - 1
                        "0010101100000001", -- R16 <- R16 OR R17
                        "1001011001111111", -- R31:R30 <- R31:R30 + $1F
                        "1001011001111111", -- R31:R30 <- R31:R30 + $1F
                        "1001010000001000", -- SReg(0) <- '1'
                        "1111101100000000", -- T <- R16(0)
                        "0001111111100001");-- R30 <- R30 + R17

            casesIn := (X"00",              -- $00 AND XX
                        X"00",              -- $00 AND XX
                        X"00",              -- $00 AND XX
                        X"00",              -- $00 AND XX
                        X"0F",              -- $0F OR $00
                        X"0F",              -- $0F +  $00
                        X"0E",              -- $0F DEC
                        X"0F",              -- $0E OR $0F
                        X"1F",              -- $00 + $1F low byte
                        X"00",              -- $00 + $1F low byte
                        X"FF",              -- reg outputs $FF, bitmask sets
                        X"0F",              -- reg outputs $0F, bitmask sets
                        X"2E");             -- $1F + $0F
            casesA  := ("--------",         -- undef
                        "--------",         -- undef
                        "--------",         -- undef
                        "--------",         -- undef
                        X"00",              -- 
                        X"00",              -- 
                        X"0F",              -- 
                        X"0E",              -- 
                        X"00",              -- 
                        X"00",              -- 
                        "--------",         -- undef
                        X"0F",              -- 
                        X"1F");             -- 
            casesB  := (X"00",              -- 
                        X"00",              -- 
                        X"00",              -- 
                        X"00",              -- 
                        X"0F",              -- 
                        X"0F",              -- 
                        X"01",              -- 
                        X"0F",              -- 
                        X"1F",              -- 
                        X"00",              -- 
                        X"0F",              -- 
                        X"0F",              -- 
                        X"0F");             -- 

            -- do first operation in test vectors
            IR <= IRTest(TEST_SIZE);
            wait for CLK_PERIOD*0.50;
            RegIn <= casesIn(TEST_SIZE);
            wait for CLK_PERIOD*0.5;

            -- do next operation, checking values from last operation
            for i in TEST_SIZE-1 downto 0 loop
                IR <= IRTest(i);
                wait for CLK_PERIOD*0.50;
                RegIn <= casesIn(i);
                wait for CLK_PERIOD*0.4;
                -- check result A right before next clk rise
                assert (std_match(casesA(i), RegAOut))
                    report  "A reg failure at test " & integer'image(TEST_SIZE-i)
                    severity  ERROR;
                -- check result B right before next clk rise
                assert (std_match(casesB(i), RegBOut))
                    report  "B reg failure at test " & integer'image(TEST_SIZE-i)
                    severity ERROR;
                wait for CLK_PERIOD*0.1;
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