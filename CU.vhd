 ----------------------------------------------------------------------------
--
--
-- Control Unit
--
-- RISC Control Unit for the AVR CPU. This contains the 16-bit instruction
-- register, logic for instruction decoding, and a finite state machine for
-- instruction cycle counts. It outputs all the necessary control signals for
-- executing instructions, including addressing, ALU operations, register
-- operations,
--
-- Inputs:
-- ProgDB  - program memory data bus
-- IR      - instruction register input
-- SReg    - status flags
-- load    - load output to tell IR register when to fetch new instruction
-- CLK         - system clock
-- RegIoFlag   - '1' if external address is in register range
-- RegIoSelFlag- external address is in either io or general range
-- DataAB      - address to be remapped to registers
--
-- Outputs:
--
-- Immed       - immediate value K
-- ImmedEn     - mux ctrl signal for immed into ALU A Reg
-- RegWSel     - register write select
-- RegSelA     - register A select
-- RegSelB     - register B select
-- IORegWEn    - IO register write enable
-- IORegWSel   - IO register write select
-- IndWEn      - Indirect Addr write enable
-- IndAddrSel  - Indirect Addr write select
-- IOOutSel    - Mux ctrl signal for outputting IO reg to A Reg
-- DataRd      - indicates data memory is being read, active lo
-- DataWr      - indicates data memory is being written, active lo
-- IORegOutEn  - OUT command enable TODO delete
-- ALUaddsub   - ALU adder/subber operation signals
-- ALUsr       - ALU shifter/rotator operation signals
-- ALUfop      - ALU F Block operation signals
-- ALUcomneg   - ALU com/neg operation signals
-- ALUSel      - ALU output select
-- bitmask     - mask for writing to flags (SReg)
-- CPC         - bit for signalling to ALU when CPC is the op
-- LoadIn      - selects data line into reg
-- SRegLd          - select line to mux status reg source
-- DataOffsetSel   - data address offset source select
-- PreSel          - data pre/post address select
-- QOffset         - address offset for data memory unit
-- DataDBWEn       - DataDB write enable
-- DataABMux       - DataAB mux control signal
--
-- Revision History:
-- 01/24/2019   Sophia Liu      Initial revision
-- 01/29/2019   Sundar Pandian  Testing Git, branches
-- 01/30/2019   Sundar Pandian  Initial architecture writeup
-- 01/31/2019   Sundar Pandian  added bitmask support
-- 02/01/2019   Sundar Pandian  debugged with testbench support
-- 02/06/2019   Sundar Pandian  Rewrote to match Glen's preferred structure
-- 02/07/2019   Sundar Pandian  Started adding support for AVR load/store instr
-- 02/09/2019   Sundar Pandian  Added documentation for AVR load/store ops
-- 02/09/2019   Sophia Liu      Updated and cleaned doc 
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

use work.opcodes.all;
use work.constants.all;

entity CU is
    port(
        ProgDB  : in std_logic_vector(ADDRSIZE-1 downto 0);     -- program memory data bus
        IR      : in std_logic_vector(IRSIZE-1 downto 0);       -- instruction register input
        SReg    : in std_logic_vector(REGSIZE-1 downto 0);      -- status flags
        load    : buffer std_logic;                             -- load output to tell IR register
                                                                --  when to fetch new instruction

        Immed       : out std_logic_vector(REGSIZE-1 downto 0); -- immediate value K
        ImmedEn     : out std_logic;                            -- mux ctrl signal for immed into ALU A Reg

        -- to register unit
        RegWEn      : out std_logic;                                -- register write enable
        RegWSel     : out std_logic_vector(RADDRSIZE-1 downto 0);   -- register write select
        RegSelA     : out std_logic_vector(RADDRSIZE-1 downto 0);   -- register A select
        RegSelB     : out std_logic_vector(RADDRSIZE-1 downto 0);   -- register B select
        IORegWEn    : out std_logic;                                -- IO register write enable
        IORegWSel   : out std_logic_vector(IOADDRSIZE-1 downto 0);  -- IO register write select
        IndWEn      : out std_logic;                                -- Indirect Addr write enable
        IndAddrSel  : out ADDR_SEL;                                 -- Indirect Addr write select
        IOOutSel    : out std_logic;                                -- Mux ctrl signal for outputting IO reg to A Reg

        DataRd          : out std_logic;                        -- indicates data memory is being read, active lo
        DataWr          : out std_logic;                        -- indicates data memory is being written, active lo

        IORegOutEn  : out   std_logic;                          -- OUT command enable TODO delete

        -- to ALU and SReg
        ALUaddsub   : out ALU_ADDSUB;                           -- ALU adder/subber operation signals
        ALUsr       : out ALU_SR;                               -- ALU shifter/rotator operation signals
        ALUfop      : out ALU_FOPS;                             -- ALU F Block operation signals
        ALUcomneg   : out ALU_COMNEG;                           -- ALU com/neg operation signals
        ALUSel      : out ALU_SELECTS;                          -- ALU output select
        bitmask     : out BIT_MASK;                             -- mask for writing to flags (SReg)
        CPC         : out std_logic;                            -- bit for signalling to ALU when CPC is the op

        LoadIn      : out LOADIN_SEL;                           -- selects data line into reg
        SRegLd      : out   std_logic;                          -- select line to mux status reg source

        -- Data memory access
        DataOffsetSel   : out OFFSET_SEL;                       -- data address offset source select
        PreSel          : out PREPOST_ADDR;                     -- data pre/post address select
        QOffset         : out std_logic_vector(Q_OFFSET_SIZE-1 downto 0); -- address offset for data memory unit
        DataDBWEn       : out std_logic;                        -- DataDB write enable
        DataABMux       : out std_logic;                        -- DataAB mux control signal

        CLK         : in std_logic;                           -- system clock
        RegIoFlag   : in std_logic;                           -- '1' if external address is in register range
        RegIoSelFlag: in std_logic;                           -- external address is in either io or general range
        DataAB      : in std_logic_vector(5 downto 0)         -- address to be remapped to registers
    );

end CU;

--
--  CU Architecture
--

architecture RISC of CU is
    signal cycle_num    :   OP_CYCLE := ZERO_CYCLES;                -- TODO delete
    signal cycle        :   std_logic_vector(1 downto 0) := "00";   -- TODO delete
begin

    -- asynchronously decodes IR inputs
    decoder : process(IR, CLK)
    begin
            -- sets cycle number for op_codes
            -- defaults operations to 1 cycle
            cycle_num <= ONE_CYCLE;
            -- control signals default values, reset all write signal
            -- this way each operation only enables writes if necessary
            RegWEn      <= WRITE_DIS;
            IORegWEn    <= WRITE_DIS;
            IndWEn      <= WRITE_DIS;
            DataDBWEn   <= WRITE_DIS;
            -- disable immediate value muxing into ALU by default
            ImmedEn     <= IMM_DIS;
            -- defaults to no bit setting in SReg
            bitmask     <= MASK_NONE;
            -- defaults to not CPC operation
            CPC         <= CPC_RST;
            -- set F Block to pass through B Register Value by default
            ALUfop      <= FOP_B;
            -- set COMNEG block to pass through A Register Value by default
            ALUcomneg   <= COMNEG_NONE;
            -- default writing outputs from Reg A back into Reg space
            LoadIn <= LD_REGA;
            -- default to outputting from register space, not IO
            IOOutSel <= REG_A_OUT;
            -- default to loading from indirect addressing, not direct memory
            DataABMux <= IND_ADDR;
            -- default DataRd and DataWr to inactive, active low signals
            DataRd <= '1';
            DataWr <= '1';
            -- default address offset value is 0
            DataOffsetSel <= ZERO_SEL;

            if  std_match(IR, OpLDX) or
                std_match(IR, OpLDXI) or
                std_match(IR, OpLDXD) or
                std_match(IR, OpLDYI) or
                std_match(IR, OpLDYD) or
                std_match(IR, OpLDZI) or
                std_match(IR, OpLDZD) then
                -- 1001000dddddoooo

                    cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                    LoadIn <= LD_DB;                    -- loading values into register space from DataDB

                    DataOffsetSel <= IR(1 downto 0);    -- offset values for 0, +1, -1 stored in low two bits of IR, IR(1..0)
                                                        -- add  0 -> "00" = ZERO_SEL
                                                        -- add +1 -> "01" = INC_SEL
                                                        -- add -1 -> "10" = DEC_SEL

                    PreSel <= IR(1);                    -- pre flag setting stored in IR(1)
                                                        -- pre-op -> IR(1) = '1' = PRE_ADDR
                                                        -- pre-op -> IR(0) = '0' = POST_ADDR

                    IndAddrSel <= IR(3 downto 2);       -- indirect addressing stored in IR(3..2)
                                                        -- X -> IR(3..2) = "11" = X_SEL
                                                        -- Y -> IR(3..2) = "10" = Y_SEL
                                                        -- Z -> IR(3..2) = "00" = Z_SEL

                    RegWSel <= IR(8 downto 4);          -- Operand 1 is the register being written to, loc in IR(8..4)

                    if cycle = ZERO_CYCLES then         -- during first cycle
                        -- do nothing
                    else                                -- during second cycle
                        IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg

                        DataRd <= CLK;                  -- DataRd = CLK for the second cycle, so will go active low at end

                        RegWEn <= WRITE_EN;             -- Write data from DataDB into register space
                    end if;
            end if;

            if  std_match(IR, OpLDDY) or std_match(IR, OpLDDZ) then
                -- 10q0qq0dddddoqqq

                    cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                    LoadIn <= LD_DB;                    -- loading values into register space from DataDB

                                                        -- offset values is the q offset, encoded in the IR
                                                        -- all q bits as seen above: IR(13)&IR(11..10)&IR(2..0)
                    QOffset <= IR(13) & IR(11 downto 10) & IR(2 downto 0);

                    DataOffsetSel <= OFFS_SEL;          -- Data Offset is the q offset value

                    PreSel <= PRE_ADDR;                 -- pre flag set to output to address bus whenever add op finished

                    IndAddrSel <= IR(3) & '0';          -- indirect addressing stored in IR(3), while setting LSB = '0'
                                                        -- Y -> IR(3) & '0' = "10" = Y_SEL
                                                        -- Z -> IR(3) & '0' = "00" = Z_SEL

                    RegWSel <= IR(8 downto 4);          -- Operand 1 is the register being written to, loc in IR(8..4)

                    if cycle = ZERO_CYCLES then         -- during first cycle
                        -- do nothing
                    else                                -- during second cycle
                        DataRd <= CLK;                  -- DataRd = CLK for the second cycle, so will go active low at end

                        RegWEn <= WRITE_EN;             -- Write data from DataDB into register space
                    end if;
            end if;

            if  std_match(IR, OpLDS) then
                -- 1001000ddddd0000

                    cycle_num <= THREE_CYCLES;          -- takes 3 cycles to complete operation

                    LoadIn <= LD_DB;                    -- loading values into register space from DataDB


                    RegWSel <= IR(8 downto 4);          -- Operand 1 is the register being written to, loc in IR(8..4)

                    if cycle = ZERO_CYCLES then         -- during first cycle
                        -- do nothing
                    elsif cycle = ONE_CYCLE then        -- during second cycle
                        DataABMux <= MEM_ADDR;          -- signal to latch and output ProgDB memory on DataAB
                    else                                -- during third cycle
                        DataABMux <= MEM_ADDR;          -- still outputting ProgDB to DataAB

                        DataRd <= CLK;                  -- DataRd = CLK for the third cycle, so will go active low at end

                        RegWEn <= WRITE_EN;             -- write data from DataDB into register
                    end if;
            end if;

            if  std_match(IR, OpSTX) or
                std_match(IR, OpSTXI) or
                std_match(IR, OpSTXD) or
                std_match(IR, OpSTYI) or
                std_match(IR, OpSTYD) or
                std_match(IR, OpSTZI) or
                std_match(IR, OpSTZD) then
                -- 1001001dddddoooo

                    cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                    -- loading values from RegA into DataDB so no change from default

                    DataOffsetSel <= IR(1 downto 0);    -- offset values for 0, +1, -1 stored in low two bits of IR, IR(1..0)
                                                        -- add  0 -> "00" = ZERO_SEL
                                                        -- add +1 -> "01" = INC_SEL
                                                        -- add -1 -> "10" = DEC_SEL

                    PreSel <= IR(1);                    -- pre flag setting stored in IR(1)
                                                        -- pre-op -> IR(1) = '1' = PRE_ADDR
                                                        -- pre-op -> IR(0) = '0' = POST_ADDR

                    IndAddrSel <= IR(3 downto 2);       -- indirect addressing stored in IR(3..2)
                                                        -- X -> IR(3..2) = "11" = X_SEL
                                                        -- Y -> IR(3..2) = "10" = Y_SEL
                                                        -- Z -> IR(3..2) = "00" = Z_SEL

                    RegSelA <= IR(8 downto 4);          -- Operand 1 is the register being read from, loc in IR(8..4)

                    if cycle = ZERO_CYCLES then         -- during first cycle
                        -- do nothing
                    else                                -- during second cycle
                        IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg

                        DataWr <= CLK;                  -- DataWr = CLK for the second cycle, so will go active low at end

                        DataDBWEn <= WRITE_EN;          -- Write data from register into DataDB
                    end if;
            end if;

            if  std_match(IR, OpSTDZ) or std_match(IR, OpSTDY) then
                -- 10q0qq1rrrrroqqq

                    cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                    -- loading values from RegA into DataDB so no change from default

                    -- offset values is the q offset, encoded in the IR
                    -- all q bits as seen above: IR(13)&IR(11..10)&IR(2..0)
                                        QOffset <= IR(13) & IR(11 downto 10) & IR(2 downto 0);

                    DataOffsetSel <= OFFS_SEL;          -- Data Offset is the q offset value

                    PreSel <= PRE_ADDR;                 -- pre flag set to output to address bus whenever add op finished

                    IndAddrSel <= IR(3) & '0';          -- indirect addressing stored in IR(3), while setting LSB = '0'
                                                        -- Y -> IR(3) & '0' = "10" = Y_SEL
                                                        -- Z -> IR(3) & '0' = "00" = Z_SEL

                    RegSelA <= IR(8 downto 4);          -- Operand 1 is the register being read from, loc in IR(8..4)

                    if cycle = ZERO_CYCLES then         -- during first cycle
                        -- do nothing
                    else                                -- during second cycle
                        DataWr <= CLK;                  -- DataRd = CLK for the third cycle, so will go active low at end

                        DataDBWEn <= WRITE_EN;          -- write data from register into DataDB
                    end if;
            end if;

            if  std_match(IR, OpSTS) then
                -- 1001001rrrrr0000

                    cycle_num <= THREE_CYCLES;          -- takes 3 cycles to complete operation

                    -- loading values into DataDB from RegA so no change from default

                    RegSelA <= IR(8 downto 4);          -- Operand 1 is the register being written to, loc in IR(8..4)

                    if cycle = ZERO_CYCLES then         -- during first cycle
                        -- do nothing
                    elsif cycle = ONE_CYCLE then        -- during second cycle
                        DataABMux <= MEM_ADDR;          -- signal to latch and output ProgDB memory on DataAB
                    else                                -- during third cycle
                        DataABMux <= MEM_ADDR;          -- still outputting ProgDB to DataAB

                        DataWr <= CLK;                  -- DataWr = CLK for the third cycle, so will go active low at end

                        DataDBWEn <= WRITE_EN;          -- write data from mem space addr into DataDB
                    end if;
            end if;

            if  std_match(IR, OpLDI) then
                -- 1110kkkkddddkkkk
                    -- takes 1 cycle to complete operation so no change from default

                    LoadIn <= LD_IMM;                   -- loading values into register space from Immed

                    RegWSel <= '1' & IR(7 downto 4);    -- Operand 1 is the register being written to
                                                        -- Immediate operations limited to upper half of register space
                                                        -- so, MSB of RADDRSIZE = '1' and rest is loc in IR(7..4)

                                                        -- immediate value found in k locs in IR, IR(11..8)&IR(3..0)
                    Immed <= IR(11 downto 8) & IR(3 downto 0);

                    RegWEn <= WRITE_EN;                 -- write data from Immed into register space
            end if;

            if  std_match(IR, OpPOP) then
                -- 1001000ddddd1111

                    cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                    LoadIn <= LD_DB;                    -- loading values into register space from DataDB

                    DataOffsetSel <= INC_SEL;           -- Popping pre-increments
                    PreSel <= PRE_ADDR;                 --  the Stack Pointer

                    IndAddrSel <= SP_SEL;               -- indirect addressing stored in SP

                    RegWSel <= IR(8 downto 4);          -- Operand 1 is the register being written to, loc in IR(8..4)

                    if cycle = ZERO_CYCLES then         -- during first cycle
                        -- do nothing
                    else                                -- during second cycle
                        IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg

                        DataRd <= CLK;                  -- DataRd = CLK for the second cycle, so will go active low at end

                        RegWEn <= WRITE_EN;             -- Write data from DataDB into register space
                    end if;
            end if;

            if  std_match(IR, OpPUSH) then
                -- 1001001rrrrr1111

                    cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                    -- loading values from RegA into DataDB so no change from default

                    DataOffsetSel <= DEC_SEL;           -- Pushing post decrements
                    PreSel <= POST_ADDR;                --  the Stack Pointer

                    IndAddrSel <= SP_SEL;               -- indirect addressing stored in Stack Pointer

                    RegSelA <= IR(8 downto 4);          -- Operand 1 is the register being read from, loc in IR(8..4)
                    -- during first cycle
                    if cycle = ZERO_CYCLES then
                        -- do nothing
                    else                                -- during second cycle
                        IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg

                        DataWr <= CLK;                  -- DataWr = CLK for the second cycle, so will go active low at end

                        DataDBWEn <= WRITE_EN;          -- Write data from register into DataDB
                    end if;
            end if;

            if  std_match(IR, OpMOV) then
                --001011rdddddrrrr
                    -- takes 1 cycle to complete operation so no change from default

                    -- loading values from RegA into RegB so no change from default loading

                    RegWSel <= IR(8 downto 4);          -- Operand 1 is the register being written to, loc in IR(8..4)

                    RegSelA <= IR(9) & IR(3 downto 0);  -- Operand 2 is the register being read from, loc in IR(9)&IR(3..0)

                    RegWEn <= WRITE_EN;                 -- writing to register
            end if;

            -- handle changing address source
            if  RegIoFlag = '1' then
                if (std_match(IR, OpSTS) or
                std_match(IR, OpSTDZ) or
                std_match(IR, OpSTDY) or
                std_match(IR, OpSTX) or
                std_match(IR, OpSTXI) or
                std_match(IR, OpSTXD) or
                std_match(IR, OpSTYI) or
                std_match(IR, OpSTYD) or
                std_match(IR, OpSTZI) or
                std_match(IR, OpSTZD)) then
                -- writing to reg/io at data address
                if cycle /= ZERO_CYCLES then
                    DataDBWEn <= WRITE_DIS; -- disable data db
                    DataWr <= '1';          -- not writing to data db
                    if RegIoSelFlag = REG_A_OUT then    -- if writing to register
                        RegWEn <= WRITE_EN;             -- enable reg writing
                        RegWSel <= dataAB(4 downto 0);  -- set reg address
                    else                                -- if writing to io
                        IORegWEn <= WRITE_EN;           -- enable io writing
                        IORegWSel <= dataAB(5 downto 0); -- set io address
                    end if;
                end if;

               elsif std_match(IR, OpLDS) or
                std_match(IR, OpLDDY) or
                std_match(IR, OpLDDZ) or
                std_match(IR, OpLDX) or
                std_match(IR, OpLDXI) or
                std_match(IR, OpLDXD) or
                std_match(IR, OpLDYI) or
                std_match(IR, OpLDYD) or
                std_match(IR, OpLDZI) or
                std_match(IR, OpLDZD) then
               -- reading from reg at data address
               LoadIn <= LD_REGA;           -- set load source to registers
               if cycle /= ZERO_CYCLES then
                    DataRd <= '1';          -- not reading from db
                    if RegIoSelFlag = REG_A_OUT then    -- if reading from register
                        RegSelA <= dataAB(4 downto 0);  -- set reg address
                    else                                -- otherwise reading from io
                        IORegWSel <= dataAB(5 downto 0); -- set io address
                        IOOutSel <= IO_OUTPUT;           -- set reg unit output to io regs
                    end if;
                end if;
                end if;
            end if;

    end process decoder;

    -- load enable signal telling when to fetch next instruction
    -- cycle value is zero indexed so finaly value is one less than cycle_num
    load <= '1' when cycle = cycle_num-1 else
            '0';

    -- cycle counter, only operates when cycle_num /= 1
    FSM_noSM : process (CLK)
    begin
      if (rising_edge(CLK)) then
            if load = '0' then
                cycle <= cycle + 1;
            else
                cycle <= "00";
            end if;
      end if;
    end process FSM_noSM;

end RISC;
