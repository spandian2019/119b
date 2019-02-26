 ----------------------------------------------------------------------------
--
--
-- Control Unit
--
-- RISC Control Unit for the AVR CPU. This contains the 16-bit instruction
-- register and logic for instruction decoding. The main inputs are from the
-- program data bus and status register, and it outputs the necessary control
-- signals for executing instructions, including data and program addressing,
-- ALU operations, status register controls, and register and IO operations.
-- For each decoded instruction, the appropriate control signals are outputted
-- for each cycle in the instruction. 
--
-- Inputs:
--
-- ProgDB       - 16 bit program memory data bus
-- SReg         - 8 bit status flag bus
-- ZeroFlag     - 1 bit zero flag from ALU for CPSE op
-- SBFlag       - 1 bit skip bit flag from ALU for SBRC/SBRS op
-- CLK          - 1 bit system clock
-- RegIoFlag    - 1 bit flag, '1' if external address is in register range
-- RegIoSelFlag - 1 bit flag, external address is in either io or general range
-- DataAB       - 6 bit address to be remapped to registers
--
-- Outputs:
--
-- Immed        - 8 bit immediate value K
-- ImmedEn      - 1 bit mux ctrl signal for immed into ALU A Reg
-- RegWSel      - 5 bit register write select
-- RegSelA      - 5 bit register A select
-- RegSelB      - 5 bit register B select
-- IORegWEn     - 1 bit IO register write enable
-- IORegWSel    - 6 bit IO register write select
-- IndWEn       - 1 bit Indirect Addr write enable
-- IndAddrSel   - 1 bit Indirect Addr write select
-- IOOutSel     - 2 bit Mux ctrl signal for outputting IO reg to A Reg
-- DataRd       - 1 bit ctrl, indicates data memory is being read, active lo
-- DataWr       - 1 bit ctrl, indicates data memory is being written, active lo
-- IORegOutEn   - 1 bit IO command enable
-- ALUaddsub    - 3 bit ALU adder/subber operation signals
-- ALUsr        - 2 bit ALU shifter/rotator operation signals
-- ALUfop       - 4 bit ALU F Block operation signals
-- ALUcomneg    - 2 bit ALU com/neg operation signals
-- ALUSel       - 3 bit ALU output select
-- bitmask      - 8 bit mask for writing to status flags (SReg)
-- CPC          - 1 bit ctrl for signalling to ALU when CPC is the op
-- LoadIn       - 3 bit ctrl, selects data line into reg
-- SRegLd          - 1 bit select line to mux status reg source
-- DataOffsetSel   - 2 bit data address offset source select
-- PreSel          - 1 bit data pre/post address select
-- QOffset         - 6 bit address offset for data memory unit
-- DataDBWEn       - 1 bit DataDB write enable
-- DataABMux       - 1 bit DataAB mux control signal
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
        SReg    : in std_logic_vector(REGSIZE-1 downto 0);      -- status flags
        ZeroFlag: in std_logic;                                 -- zero flag from ALU for CPSE op
        SBFlag  : in std_logic;                                 -- skip bit flag from ALU for SBRC/SBRS op

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
        DataAB      : in std_logic_vector(5 downto 0);        -- address to be remapped to registers

        -- Prog memory access
        Load            : out std_logic;
        ProgSourceSel   : out SOURCE_SEL;
        ProgIRSource    : out std_logic_vector(ADDRSIZE-1 downto 0)
    );

end CU;

--
--  CU Architecture
--

architecture RISC of CU is
    signal cycle_num    :   OP_CYCLE := ZERO_CYCLES;                -- TODO delete
    signal cycle        :   std_logic_vector(1 downto 0) := "00";   -- TODO delete

    signal IR           :   std_logic_vector(ADDRSIZE-1 downto 0);  -- instruction register

    signal ProgDBLatch  :   std_logic_vector(ADDRSIZE-1 downto 0);  -- Prog Address Bus latch
begin

    -- asynchronously decodes IR inputs
    decoder : process(IR, CLK, cycle, SBFlag, ZeroFlag, cycle_num, RegIoFlag, RegIoSelFlag, SReg, DataAB, ProgDB, ProgDBLatch)
    begin
            -- sets cycle number for op_codes
            -- defaults operations to 1 cycle
            cycle_num   <= ONE_CYCLE;
            -- control signals default values, reset all write signal
            -- this way each operation only enables writes if necessary
            RegWEn      <= WRITE_DIS;
            IORegWEn    <= WRITE_DIS;
            IndWEn      <= WRITE_DIS;
            DataDBWEn   <= WRITE_DIS;
            -- disable immediate value muxing into ALU by default
            ImmedEn     <= IMM_DIS;
            -- default Immed register to IR(11..8) & IR(3..0)
            --  because general IR form follows: ----KKKK----KKKK
            Immed       <= IR(11 downto 8) & IR(3 downto 0);
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
            -- default PC to increment when operation is on last cycle
            --  else no change to PC
            if cycle = cycle_num - 1 then
                ProgSourceSel <= NORMAL_SRC;
            else
                ProgSourceSel <= RST_SRC;
            end if;
            -- load defaults to indirect addressing
            load <= '1';
            -- clear IO register write select line, allows for ALU default writing to SReg
            IORegWSel <= (others => '0');

            -- considering single byte adder/subber ops:
            -- ADC, ADD, SBC, SUB, CPC, CP
            -- all of these have the top three bits in IR cleared
            -- can be combined with CPSE
            if  std_match(IR, OpADC) or std_match(IR, OpADD) or
                std_match(IR, OpSBC) or std_match(IR, OpSUB) or
                std_match(IR, OpCPC) or std_match(IR, OpCP)  then
            --  000ooordddddrrrr

                LoadIn <= LD_ALU;

                RegSelA <= IR(8 downto 4);          -- Operand 1 is the first addend, loc in IR(8..4)

                RegWSel <= IR(8 downto 4);          -- Operand 1 is the register being written to

                RegSelB <= IR(9) & IR(3 downto 0);  -- Operand 2 is the second addend, loc in IR(9)&IR(3..0)

                BitMask <= MASK_ADD;

                ALUSel <= ADDSUBOUT;                -- enable Adder/Subber output

                RegWEn <= IR(11);                   -- Writes data from RegA, mapped in IR(11)
                                                    -- only not set for CP, CPC. Don't rewrite reg value

                                                    -- subber flag mapped in IR as function of 2 bits
                ALUaddsub(SUBFLAG)  <= IR(11) xor IR(10);
                                                    -- carry/nborrow bit mapped in IR
                if (IR(12) xor IR(11) xor IR(10)) = '1' then
                                                    -- signal to ALU adder to use carry bit
                    if (IR(11) xor IR(10)) = '1' then
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= NCARRY_IN;
                        if IR(10) = '1' then
                            CPC <= CPC_SET;
                        end if;
                    else
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= CARRY_IN;
                    end if;
                else
                    -- all no carry operations use same logic block with
                    -- carry in mapped in IR
                    --  clear active-hi carry for add
                    --  clear active-lo borrow for sub
                    -- maps same as to subFlag
                    if (IR(11) xor IR(10)) = '1' then
                                                    -- clearing borrow for subber, sets carry
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;
                    else
                                                    -- clearing carry for adder, resets carry
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= RST_CARRY;
                    end if;
                end if;
            end if;

            -- considering word adder/subber ops
            if  std_match(IR, OpADIW) or std_match(IR, OpSBIW) then
            --  1001011oKKddKKKK  -- IR

                LoadIn <= LD_ALU;

                RegWEn <= WRITE_EN;

                BitMask <= MASK_ADIW;

                cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                ALUSel <= ADDSUBOUT;                -- enable Adder/Subber operation

                ALUaddsub(subFlag) <= IR(8);        -- subFlag mapped in IR

                ImmedEn <= IMM_EN;                  -- immediate value loads into second operand

                -- value in IR is offset added to register 24
                --  possible operands include {24, 26, 28, 30}
                --  low byte operation uses above bytes while high byte
                --    operation uses the next highest byte

                -- if just loaded IR, doing first cycle
                if cycle = ZERO_CYCLES then
                    -- mapping to immediate value in IR, max value of 63
                    Immed <= "00" & IR(7 downto 6) & IR(3 downto 0);
                    -- add/sub op mapped in
                    ALUaddsub(subFlag)  <= IR(8);
                    -- carry/nborrow cleared
                    if IR(8) = '1' then
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;
                    else
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= RST_CARRY;
                    end if;
                    -- limits operand addresses
                    RegSelA <= "11" & IR(5 downto 4) & '0';
                    RegWSel <= "11" & IR(5 downto 4) & '0';
                else
                    ALUfop <= FOP_ZERO;             -- add in zero
                    if IR(8) = '1' then
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= NCARRY_IN;
                    else
                        -- carry out from low byte carries in to high byte add
                        ALUaddsub(CARRY_S1 downto CARRY_S0) <= CARRY_IN;
                    end if;
                    -- previous operand addresses + 1
                    RegSelA <= "11" & IR(5 downto 4) & '1';
                    RegWSel <= "11" & IR(5 downto 4) & '1';
                end if;
            end if;

            ---- considering word multiply op
            --if  std_match(IR, OpMUL) then

            --    LoadIn <= LD_ALU;

            --    RegWEn <= WRITE_EN;

            --    -- takes 2 cycles to complete operation
            --    cycle_num <= "10";
            --    -- enable MUL operation
            --    ALUSel <= MulEn;

            --    -- output of MUL op is saved in R1:R0
            --    --  low byte operation uses above bytes while high byte
            --    --    operation uses the next highest byte

            --    RegSelB <= IR(9) & IR(3 downto 0);
            --    -- first do low byte multiply
            --    if cycle = "00" then
            --        RegWSel <= "00000";
            --    elsif cycle = "01" then
            --        RegWSel <= "00001";
            --    end if;

            --    BitMask <= MASK_MUL;
            --end if;

            -- considering immediate subber operations
            if  std_match(IR, OpSUBI) or std_match(IR, OpSBCI) or std_match(IR, OpCPI) then
                -- 0oooKKKKddddKKKK

                LoadIn <= LD_ALU;

                ALUSel <= ADDSUBOUT;                -- enable Adder/Subber operation

                -- carry/nborrow bit mapped in IR
                if IR(12) = '0' then
                    -- send carry bit to ALU
                    ALUaddsub(CARRY_S1 downto CARRY_S0) <= NCARRY_IN;
                else
                    -- all no carry operations use same logic block with
                    -- carry in mapped in IR
                    --  clear active-hi carry for add
                    --  clear active-lo borrow for sub
                    -- maps same as to subFlag
                    ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;
                end if;
                -- subbing so subFlag active
                ALUaddsub(subFlag)  <= OP_SUB;
                -- CPI doesn't rewrite register, mapped in IR
                RegWEn <= IR(14);
                -- immediate value loads into second operand
                ImmedEn <= IMM_EN;
                RegSelA <= '1' & IR(7 downto 4);
                RegWSel <= '1' & IR(7 downto 4);
                BitMask <= MASK_ADD;
            end if;

            -- considering incrementing/decrementing operations
            if  std_match(IR, OpINC) or std_match(IR, OpDEC) then
                -- 1001010dddddoo1o

                RegWEn <= WRITE_EN;

                LoadIn <= LD_ALU;

                ALUSel <= ADDSUBOUT;                -- enable Adder/Subber operation

                -- add/sub conditional mapped in IR
                ALUaddsub(subFlag)  <= OP_ADD;
                if IR(3) = '1' then
                    ALUfop <= FOP_ONES;
                    ALUaddsub(CARRY_S1 downto CARRY_S0) <= RST_CARRY;   -- set carry flag
                else
                    ALUfop <= FOP_ZERO;
                    ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;   -- set carry flag
                end if;

                ImmedEn <= IMM_EN;                  -- immediate value loads into second operand

                RegSelA <= IR(8 downto 4);          -- Operand 1 is the first addend, loc in IR(8..4)

                RegWSel <= IR(8 downto 4);          -- Operand 1 is the register being written to

                BitMask <= MASK_DECINC;
            end if;

            -- considering COM and NEG operations
            if  std_match(IR, OpCOM) or std_match(IR, OpNEG) then
                -- 1001010ddddd000o

                LoadIn <= LD_ALU;

                ALUSel <= ADDSUBOUT;                -- enable Adder/Subber operation

                RegWEn <= WRITE_EN;

                -- carry in mapped in IR
                --  clear active-hi carry for add
                --  clear active-lo borrow for sub
                -- clear nborrow
                ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;
                -- always subbing so set
                ALUaddsub(SUBFLAG)  <= OP_SUB;
                -- either subtract operand from xFF or x00
                -- xFF for NEG
                -- x00 for COM
                if IR(0) = '0' then
                    ALUcomneg <= ALU_COM;
                else
                    ALUcomneg <= ALU_NEG;
                end if;

                RegSelB <= IR(8 downto 4);

                RegWSel <= IR(8 downto 4);
                -- set bitmask based on if COM op or NEG op
                if IR(0) = '0' then
                    BitMask <= MASK_COM;
                else
                    BitMask <= MASK_NEG;
                end if;
            end if;

            if  std_match(IR, OpAND) or std_match(IR, OpANDI) then
                -- 001000rdddddrrrr
                -- 0111KKKKddddKKKK

                LoadIn <= LD_ALU;

                ALUSel <= FBLOCKOUT;                -- enable F Block Operation

                RegWEn <= WRITE_EN;

                ALUfop <= FOP_AND;                  -- select AND operation

                if IR(14) = '1' then
                    -- immediate value loads into second operand if ANDI op
                    ImmedEn <= IMM_EN;
                end if;

                RegWSel <= IR(8 downto 4);
                RegSelA <= IR(8 downto 4);
                -- ANDI operation only maps to upper half of register space
                if IR(14) = '1' then
                    RegSelA(4) <= '1';
                    RegWSel(4) <= '1';
                end if;
                RegSelB <= IR(9) & IR(3 downto 0);
                BitMask <= MASK_ANDOR;
            end if;

            if  std_match(IR, OpOR) or std_match(IR, OpORI) then

                LoadIn <= LD_ALU;

                ALUSel <= FBLOCKOUT;                -- enable F Block Operation

                RegWEn <= WRITE_EN;

                ALUfop <= FOP_OR;                   -- select OR operation

                if IR(14) = '1' then
                    -- immediate value loads into second operand
                    ImmedEn <= IMM_EN;
                end if;

                RegSelA <= IR(8 downto 4);
                RegWSel <= IR(8 downto 4);
                -- ORI operation only maps to upper half of register space
                if IR(14) = '1' then
                    RegSelA(4) <= '1';
                    RegWSel(4) <= '1';
                end if;
                RegSelB <= IR(9) & IR(3 downto 0);
                BitMask <= MASK_ANDOR;
            end if;

            if  std_match(IR, OpEOR) then

                LoadIn <= LD_ALU;

                ALUSel <= FBLOCKOUT;                -- enable F Block Operation

                RegWEn <= WRITE_EN;

                ALUfop <= FOP_XOR;                  -- select OR operation

                RegSelA <= IR(8 downto 4);
                RegWSel <= IR(8 downto 4);
                RegSelB <= IR(9) & IR(3 downto 0);
                BitMask <= MASK_EOR;
            end if;

            if  std_match(IR, OpLSR) then

                LoadIn <= LD_ALU;

                ALUSel <= SHIFTOUT;                 -- enable F Block Operation

                RegWEn <= WRITE_EN;

                -- select LSR operation
                ALUsr <= SR_LSR;
                RegSelA <= IR(8 downto 4);
                RegWSel <= IR(8 downto 4);
                BitMask <= MASK_SHIFT;
            end if;

            if  std_match(IR, OpASR) then

                LoadIn <= LD_ALU;

                ALUSel <= SHIFTOUT;                 -- enable F Block Operation

                RegWEn <= WRITE_EN;
                -- select ASR operation
                ALUsr <= SR_ASR;
                RegSelA <= IR(8 downto 4);
                RegWSel <= IR(8 downto 4);
                BitMask <= MASK_SHIFT;
            end if;

            if  std_match(IR, OpROR) then

                LoadIn <= LD_ALU;

                ALUSel <= SHIFTOUT;                 -- enable F Block Operation

                RegWEn <= WRITE_EN;

                -- select ROR operation
                ALUsr <= SR_ROR;
                -- ROR op uses carry bit from last operation ----------- TODO
                ALUaddsub(CARRY_S1 downto CARRY_S0) <= CARRY_IN;
                RegSelA <= IR(8 downto 4);
                RegWSel <= IR(8 downto 4);
                BitMask <= MASK_SHIFT;
            end if;

            if  std_match(IR, OpBCLR) or std_match(IR, OpBSET) then

                if IR(7) = '0' then
                    ALUSel <= BSET;
                else
                    ALUSel <= BCLR;
                end if;

                bitmask <= (others => '0');
                --  then set proper bit high in bitmask
                bitmask(conv_integer(IR(6 downto 4))) <= '1';
            end if;

            if  std_match(IR, OpBLD) or std_match(IR, OpBST) then

                LoadIn <= LD_ALU;

                ImmedEn <= IMM_EN;

                ALUSel <= BOUT;

                RegSelA <= IR(8 downto 4);
                RegWSel <= IR(8 downto 4);

                -- clear bitmask
                BitMask <= (others => '0');
                if IR(T_IR) = '0' then
                    RegWEn <= WRITE_EN;
                end if;
                -- store/loads T bit
                BitMask(T_SREG) <= IR(T_IR);
            end if;

            if std_match(IR, OpSWAP) then

                LoadIn <= LD_ALU;
                ALUSel <= SWAPOUT;

                RegSelA <= IR(8 downto 4);

                RegWEn <= WRITE_EN;

                RegWSel <= IR(8 downto 4);
                BitMask <= MASK_NONE;
            end if;

            if std_match(IR, OpIN) or std_match(IR, OpOUT) then
                -- 1011oppdddddpppp
                RegSelA     <= IR(8 downto 4);
                RegWSel     <= IR(8 downto 4);
                RegWEn      <= not IR(11);

                IOOutSel    <= not IR(11);

                IORegWSel   <= IR(10 downto 9) & IR(3 downto 0);
                IORegWEn    <= IR(11);
                IORegOutEn  <= IR(11);
            end if;

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
                        ProgSourceSel <= NORMAL_SRC;    -- increment PC here so ProgAB points to next IR
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
                        ProgSourceSel <= NORMAL_SRC;    -- increment PC here so ProgAB points to next IR
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
                    --ImmedEn <= IMM_EN;
                    --ALUSel <= FBLOCKOUT;
                    --ALUfop <= FOP_B;

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
                        IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg
                        -- do nothing
                    else                                -- during second cycle

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
                        IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg
                        -- do nothing
                    else                                -- during second cycle

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

            if std_match(IR, OpJMP) then
                -- 1001010aaaaa110a
                -- aaaaaaaaaaaaaaaa

                cycle_num <= THREE_CYCLES;          -- takes 3 cycles to complete operation

                if cycle = ZERO_CYCLES then         -- during first cycle
                    ProgDBLatch <= ProgDB;          -- latch ProgDB
                elsif cycle = ONE_CYCLE then        -- during second cycle
                    ProgIRSource <= ProgDBLatch;
                    ProgSourceSel <= IR_SRC;
                    load <= '0';
                else                                -- during third cycle
                    ProgSourceSel <= RST_SRC;
                end if;
            end if;

            if std_match(IR, OpRJMP) then
                -- 1100jjjjjjjjjjjj

                cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                if cycle = ZERO_CYCLES then         -- during first cycle
                    ProgSourceSel <= NORMAL_SRC;
                else                                -- during second cycle
                    ProgIRSource(11 downto 0) <= IR(11 downto 0);
                    ProgIRSource(15 downto 12) <= (others => IR(11));
                    ProgSourceSel <= IR_SRC;
                end if;
            end if;

            if std_match(IR, OpIJMP) then
                -- 10010100XXXX1001

                cycle_num <= TWO_CYCLES;            -- takes 2 cycles to complete operation

                DataOffsetSel <= ZERO_SEL;

                IndAddrSel <= Z_SEL;

                if cycle = ZERO_CYCLES then         -- during first cycle
                    ProgSourceSel <= NORMAL_SRC;
                else                                -- during second cycle
                    ProgSourceSel <= Z_SRC;
                    load <= '0';
                end if;
            end if;

            if std_match(IR, OpCALL) then
                -- 1001010aaaaa111a
                -- aaaaaaaaaaaaaaaa

                cycle_num <= ZERO_CYCLES;           -- takes 4 cycles to complete operation

                DataOffsetSel <= DEC_SEL;           -- Pushing post decrements
                PreSel <= POST_ADDR;                --  the Stack Pointer

                IndAddrSel <= SP_SEL;               -- indirect addressing stored in Stack Pointer

                if cycle = ZERO_CYCLES then         -- during first cycle
                    ProgDBLatch <= ProgDB;          -- latch ProgDB value, Prog addr to call
                    ProgSourceSel <= NORMAL_SRC;    -- inc PC to point to next value

                elsif cycle = ONE_CYCLE then        -- during second cycle
                    ProgSourceSel <= RST_SRC;       -- hold PC value here, pointing to next op IR

                    LoadIn <= LD_PROG_HI;           -- load high byte of next IR into DataDB to
                                                    --  save into stack

                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg

                    DataWr <= CLK;                  -- DataWr = CLK for the second cycle, so will go active low at end

                    DataDBWEn <= WRITE_EN;          -- Write data from register into DataDB

                elsif cycle = TWO_CYCLES then       -- during third cycle
                    ProgSourceSel <= RST_SRC;       -- hold PC value here, pointing to next op IR

                    LoadIn <= LD_PROG_LO;           -- load high byte of next IR into DataDB to
                                                    --  save into stack

                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg

                    DataWr <= CLK;                  -- DataWr = CLK for the second cycle, so will go active low at end

                    DataDBWEn <= WRITE_EN;          -- Write data from register into DataDB

                else                                -- during fourth cycle
                    ProgIRSource <= ProgDBLatch;    -- hold ProgDB value
                    ProgSourceSel <= IR_SRC;        -- output address of subroutine
                    load <= '0';                     -- loading address, not adding
                end if;
            end if;

            if std_match(IR, OpRCALL) or std_match(IR, OpICALL) then
            -- 1101jjjjjjjjjjjj - RCALL Opcode
            -- 10010101XXXX1001 - ICALL Opcode
                cycle_num <= THREE_CYCLES;          -- takes 4 cycles to complete operation

                DataOffsetSel <= DEC_SEL;           -- Pushing post decrements
                PreSel <= POST_ADDR;                --  the Stack Pointer

                IndAddrSel <= SP_SEL;               -- indirect addressing stored in Stack Pointer

                if cycle = ZERO_CYCLES then         -- during first cycle
                    ProgSourceSel <= NORMAL_SRC;    -- hold PC value here, pointing to next op IR

                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg


                elsif cycle = ONE_CYCLE then        -- during second cycle
                    LoadIn <= LD_PROG_HI;           -- load high byte of next IR into DataDB to
                                                    --  save into stack

                    DataWr <= CLK;                  -- DataWr = CLK for the second cycle, so will go active low at end

                    DataDBWEn <= WRITE_EN;          -- Write data from register into DataDB

                    ProgSourceSel <= RST_SRC;       -- hold PC value here, pointing to next op IR

                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg


                else                                -- during third cycle
                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg
                    LoadIn <= LD_PROG_LO;           -- load high byte of next IR into DataDB to
                                                    --  save into stack

                    DataWr <= CLK;                  -- DataWr = CLK for the second cycle, so will go active low at end

                    DataDBWEn <= WRITE_EN;          -- Write data from register into DataDB


                    if IR(14) = '1' then            -- then RCALL op
                        ProgIRSource(11 downto 0) <= IR(11 downto 0);
                        ProgIRSource(15 downto 12) <= (others => IR(11));
                        ProgSourceSel <= IR_SRC;
                    else                            -- then ICALL op
                        ProgSourceSel <= Z_SRC;
                        load <= '0';
                    end if;
                end if;
            end if;

            if std_match(IR, OpRET) or std_match(IR, OpRETI) then
                -- 100101010xxo1000

                cycle_num <= ZERO_CYCLES;           -- takes 4 cycles to complete operation

                DataOffsetSel <= INC_SEL;           -- Pushing post decrements
                PreSel <= PRE_ADDR;                 --  the Stack Pointer

                IndAddrSel <= SP_SEL;               -- indirect addressing stored in Stack Pointer

                --LoadIn <= LD_DB;                    -- loading values into register space from DataDB

                if cycle = ZERO_CYCLES then         -- during first cycle

                elsif cycle = ONE_CYCLE then        -- during second cycle
                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg
                                                    --  incremented SP val is written back into SP reg
                    DataRd <= CLK;                  -- DataRd = CLK for the second cycle, so will go active low at end

                    ProgSourceSel <= DB_LO_SRC;     --
                    load <= '0';

                elsif cycle = TWO_CYCLES then       -- duri ng third cycle

                else                                -- during fourth cycle
                    IndWEn <= WRITE_EN;             -- write result of arith block back to indirect address reg
                                                    --  incremented SP val is written back into SP reg
                    DataRd <= CLK;                  -- DataRd = CLK for the second cycle, so will go active low at end

                    ProgSourceSel <= DB_HI_SRC;
                    load <= '1';

                    if IR(4) = '1' then
                        BitMask <= MASK_INT;
                    end if;
                end if;
            end if;

            if std_match(IR, OpBRBC) or std_match(IR, OpBRBS) then
                -- 11110orrrrrrrbbb
                if SReg(to_integer(unsigned(IR(2 downto 0)))) = not IR(10) then
                    --branch
                    cycle_num <= TWO_CYCLES;
                else
                    -- dont branch
                    cycle_num <= ONE_CYCLE;
                end if;

                if cycle = ZERO_CYCLES then
                    ProgSourceSel <= NORMAL_SRC;
                else
                    ProgIRSource(6 downto 0) <= IR(9 downto 3);
                    ProgIRSource(15 downto 7) <= (others => IR(9));
                    ProgSourceSel <= IR_SRC;
                end if;
            end if;

            if std_match(IR, OpCPSE) then
                --000100rdddddrrrr

                BitMask <= MASK_NONE;

                RegSelA <= IR(8 downto 4);          -- Operand 1 is the first addend, loc in IR(8..4)

                RegSelB <= IR(9) & IR(3 downto 0);  -- Operand 2 is the second addend, loc in IR(9)&IR(3..0)

                ALUSel <= ADDSUBOUT;                -- enable Adder/Subber output

                ALUaddsub(SUBFLAG)  <= OP_SUB;
                ALUaddsub(CARRY_S1 downto CARRY_S0) <= SET_CARRY;

                ProgSourceSel <= NORMAL_SRC;

                if ZeroFlag = '0' then
                    cycle_num <= ONE_CYCLE;
                else
                    if std_match(ProgDB, OpLDS) or std_match(ProgDB, OpSTS) or
                       std_match(ProgDB, OpJMP) or std_match(ProgDB, OpCALL) then
                            cycle_num <= THREE_CYCLES;
                    else
                            cycle_num <= TWO_CYCLES;
                    end if;
                end if;
            end if;

            if std_match(IR, OpSBRC) or std_match(IR, OpSBRS) then
                --111111orrrrrXbbb

                ImmedEn <= IMM_EN;

                RegSelA <= IR(8 downto 4);

                ProgSourceSel <= NORMAL_SRC;

                if SBFlag = IR(9) then
                    cycle_num <= ONE_CYCLE;
                else
                    if std_match(ProgDB, OpLDS) or std_match(ProgDB, OpSTS) or
                       std_match(ProgDB, OpJMP) or std_match(ProgDB, OpCALL) then
                            cycle_num <= THREE_CYCLES;
                    else
                            cycle_num <= TWO_CYCLES;
                    end if;
                end if;
            end if;

    end process decoder;

    -- Fetches next instruction when on last cycle of operation of previous instruction
    -- cycle value is zero indexed so final value is one less than cycle_num
    IR_update: process (CLK)
    begin
        if (rising_edge(CLK)) then
            if cycle = cycle_num-1 then
                IR <= ProgDB;
            else
                IR <= IR;
            end if;
        end if;
    end process IR_update;

    -- cycle counter, only operates when cycle_num /= 1
    FSM_noSM : process (CLK)
    begin
      if (rising_edge(CLK)) then
            if cycle /= cycle_num-1 then
                cycle <= cycle + 1;
            else
                cycle <= "00";
            end if;
      end if;
    end process FSM_noSM;

end RISC;
