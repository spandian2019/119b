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
-- Ports:
--
-- Revision History:
-- 01/24/2019   Sophia Liu      Initial revision
-- 01/29/2019   Sundar Pandian  Testing Git, branches
-- 01/30/2019   Sundar Pandian  Initial architecture writeup
-- 01/31/2019   Sundar Pandian  added bitmask support
-- 02/01/2019   Sundar Pandian  debugged with testbench support
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
--use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

use work.opcodes.all;

use work.constants.all;

entity CU is
    port(
        --ProgDB  : in std_logic_vector(15 downto 0);     -- program memory data bus
        IR      : in std_logic_vector(15 downto 0);     -- instruction register input
        SReg    : in std_logic_vector(7 downto 0);      -- status flags
        load    : buffer std_logic;                     -- load output to tell IR register
                                                        --  when to fetch new instruction
        -- to registers
        RegWEn      : out std_logic;                    -- register write enable
        RegWSel     : out std_logic_vector(4 downto 0); -- register write select
        RegSelA     : out std_logic_vector(4 downto 0); -- register A select
        RegSelB     : out std_logic_vector(4 downto 0); -- register B select
        LoadIn      : out std_logic_vector(1 downto 0); -- selects data line into reg
        LoadReg     : out std_logic_vector(1 downto 0); -- signals which reg output immediate 
                                                        --  value gets outputted through
        -- to ALU and SReg
        ALUOp   : out std_logic_vector(3 downto 0); -- operation control signals
        ALUSel  : out std_logic_vector(2 downto 0); -- operation select
        bitmask : out std_logic_vector(7 downto 0); -- mask for writing to flags (SReg)

        -- I/O
        IORegInEn   : out   std_logic;                      -- IN command enable
        IORegOutEn  : out   std_logic;                      -- OUT command enable
        SRegOut     : out   std_logic_vector(7 downto 0);   -- status reg output bus
        SRegLd      : out   std_logic;                      -- select line to mux status reg source
        K           : out   std_logic_vector(7 downto 0);   -- immediate value K

        ---- Program memory access
        --ProgAddr: out std_logic_vector(15 downto 0); -- address source for program memory unit
        --ProgLoad: out std_logic;                    -- load select for PC
        --ProgAddrSel : in std_logic_vector(1 downto 0);  -- program address source select

        ---- Data memory access
        --DataAddrSel : out std_logic_vector(1 downto 0);  -- data address source select
        --DataOffsetSel : out std_logic_vector(1 downto 0);-- data address offset source select
        --PreSel  : out std_logic; -- data pre/post address select
        --DataRd  : out std_logic; -- indicates data memory is being read
        --DataWr  : out std_logic; -- indicates data memory is being written
        --DataAddr: out std_logic_vector(15 downto 0); -- address source for data memory unit
        --AddrOffset: out std_logic_vector(5 downto 0); -- address offset for data memory unit

        ---- Stack
        --StackEn     : out std_logic; -- stack enable signal
        --StackPush   : out std_logic; -- stack push/pop control
        --Reset       : out std_logic -- active low reset signal

        CLK         : in    std_logic                       -- system clock
    );

end CU;

--
--  CU Architecture
--

architecture RISC of CU is
    signal cycle_num    :   std_logic_vector(1 downto 0) := "00";
    signal cycle        :   std_logic_vector(1 downto 0) := "00";
begin

    -- synchronously decodes IR inputs
    decoder : process (CLK)
    begin
        if (rising_edge(CLK)) then
            -- sets cycle number for op_codes
            -- defaults to 1
            cycle_num <= "01";
            -- 
            if (std_match(IR, OpADIW) or
                std_match(IR, OpSBIW) or
                std_match(IR, OpMUL)) then
                    cycle_num <= "10";
            end if;

            -- register default values
            K           <= IR(11 downto 8) & IR(3 downto 0);
            RegSelA     <= IR(8 downto 4);
            RegWEn      <= '1';
            IORegInEn   <= '0';
            IORegOutEn  <= '0';
            LoadIn      <= LdALU;
            LoadReg     <= LoadNone;
            SRegLd      <= LdSRALU;
            bitmask     <= MASK_NONE;

            -- 3 MSBs of IR for AVR all use adder/subber
            --  block for ALU ops except for those that
            --  are MULS ops. These are not supported so:
            -- considering single byte adder/subber ops
            if  std_match(IR, "000-------------") then
                -- enable Adder/Subber operation
                ALUSel <= AddSubEn;
                -- subber flag mapped in IR as function of 2 bits
                ALUOp(subFlag)  <= IR(11) xor IR(10);
                -- carry/nborrow bit mapped in IR
                if (IR(12) xor IR(11) xor IR(10)) = '0' then
                    -- send carry bit to ALU
                    ALUOp(carryBit) <= SReg(0);
                else
                    -- all no carry operations use same logic block with 
                    -- carry in mapped in IR
                    --  clear active-hi carry for add
                    --  clear active-lo borrow for sub
                    -- maps same as to subFlag
                    ALUOp(carryBit) <= IR(11) xor IR(10);
                end if;
                -- CP and CPC doesn't rewrite register value, mapped in IR
                RegWEn <= IR(11);

                RegSelB <= IR(9) & IR(3 downto 0);
                RegWSel <= IR(8 downto 4);
                BitMask <= MASK_ADD;
            end if;

            -- considering word adder/subber ops
            if  std_match(IR, OpADIW) or std_match(IR, OpSBIW) then
                -- enable Adder/Subber operation
                ALUSel <= AddSubEn;
                -- subFlag mapped in IR
                ALUOp(subFlag) <= IR(8);
                -- immediate value loads into second operand
                LoadReg <= LoadB;

                -- value in IR is offset added to register 24
                --  possible operands include {24, 26, 28, 30}
                --  low byte operation uses above bytes while high byte
                --    operation uses the next highest byte

                -- if just loaded IR, doing first cycle
                if load = '1' then
                    -- mapping to immediate value in IR, max value of 63
                    K <= "00" & IR(7 downto 6) & IR(3 downto 0);
                    -- add/sub op mapped in 
                    ALUOp(subFlag)  <= IR(8);
                    -- carry/nborrow cleared
                    ALUOp(carryBit) <= IR(8);
                    -- limits operand addresses
                    RegSelA <= "11" & IR(5 downto 4) & '0';
                    RegWSel <= "11" & IR(5 downto 4) & '0';
                elsif load = '0' then
                    -- add in 0
                    K <= (others => '0');
                    -- carry out from low byte carries in to high byte add
                    ALUOp(carryBit) <= SReg(0);
                    -- previous operand addresses + 1
                    RegSelA <= "11" & IR(5 downto 4) & '1';
                    RegWSel <= "11" & IR(5 downto 4) & '1';
                end if;
                
                BitMask <= MASK_ADIW;
            end if;

            -- considering word multiply op
            if  std_match(IR, OpMUL) then
                -- enable MUL operation
                ALUSel <= MulEn;

                -- output of MUL op is saved in R1:R0
                --  low byte operation uses above bytes while high byte
                --    operation uses the next highest byte

                RegSelB <= IR(9) & IR(3 downto 0);
                -- first do low byte multiply
                if cycle = "00" then
                    RegWSel <= "00000";
                elsif cycle = "01" then
                    RegWSel <= "00001";
                end if;
                
                BitMask <= MASK_MUL;
            end if;

            -- considering immediate subber operations
            if  std_match(IR, OpSUBI) or std_match(IR, OpSBCI) or std_match(IR, OpCPI) then
                -- enable Adder/Subber operation
                ALUSel <= AddSubEn;
                -- carry/nborrow bit mapped in IR
                if IR(12) = '1' then
                    -- send carry bit to ALU
                    ALUOp(carryBit) <= SReg(0);
                else
                    -- all no carry operations use same logic block with 
                    -- carry in mapped in IR
                    --  clear active-hi carry for add
                    --  clear active-lo borrow for sub
                    -- maps same as to subFlag
                    ALUOp(carryBit) <= '1';
                end if;
                -- subbing so subFlag active
                ALUOp(subFlag)  <= '1';
                -- CPI doesn't rewrite register, mapped in IR
                RegWEn <= IR(14);
                -- immediate value loads into second operand
                LoadReg <= LoadB;
                RegSelA <= '1' & IR(7 downto 4);
                RegWSel <= '1' & IR(7 downto 4);
                BitMask <= MASK_ADD;
            end if;

            -- considering incrementing/decrementing operations
            if  std_match(IR, OpINC) or std_match(IR, OpDEC) then
                -- enable Adder/Subber operation
                ALUSel <= AddSubEn;
                -- carry in mapped in IR
                --  clear active-hi carry for add
                --  clear active-lo borrow for sub
                -- maps same as to subFlag
                ALUOp(carryBit) <= IR(3);
                -- add/sub conditional mapped in IR
                ALUOp(subFlag)  <= IR(3);
                K <= "00000001";
                -- immediate value loads into second operand
                LoadReg <= LoadB;
                RegWSel <= IR(8 downto 4);
                BitMask <= MASK_DECINC;
            end if;

            -- considering COM and NEG operations
            if  std_match(IR, OpCOM) or std_match(IR, OpNEG) then
                -- enable Adder/Subber operation
                ALUSel <= AddSubEn;
                -- carry in mapped in IR
                --  clear active-hi carry for add
                --  clear active-lo borrow for sub
                -- clear nborrow
                ALUOp(carryBit) <= '1';
                -- always subbing so set
                ALUOp(subFlag)  <= '1';
                -- either subtract operand from xFF or x00
                -- xFF for NEG
                -- x00 for COM
                K <= (others => IR(0));
                -- immediate value loads into first operand
                LoadReg <= LoadA;
                -- data into register value from register A output
                LoadIn  <= LdRegA;
                RegWSel <= IR(8 downto 4);
                -- set bitmask based on if COM op or NEG op
                if IR(0) = '0' then
                    BitMask <= MASK_COM;
                else
                    BitMask <= MASK_NEG;
                end if;
            end if;

            if  std_match(IR, OpAND) or std_match(IR, OpANDI) then
                -- enable F Block Operation
                ALUSel <= FBlockEn;
                -- select AND operation
                ALUOp <= OP_AND;
                if IR(14) = '1' then
                    -- immediate value loads into second operand if ANDI op
                    LoadReg <= LoadB;
                end if;
                RegWSel <= IR(8 downto 4);
                -- ANDI operation only maps to upper half of register space
                if IR(14) = '1' then
                    RegSelA(4) <= '1';
                    RegWSel(4) <= '1';
                end if;
                RegSelB <= IR(9) & IR(3 downto 0);
                BitMask <= MASK_ANDOR;
            end if;

            if  std_match(IR, OpOR) or std_match(IR, OpORI) then
                -- enable F Block Operation
                ALUSel <= FBlockEn;
                -- select OR operation
                ALUOp <= OP_OR;
                if IR(14) = '1' then
                    -- immediate value loads into second operand
                    LoadReg <= LoadB;
                end if;
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
                -- enable F Block Operation
                ALUSel <= FBlockEn;
                -- select XOR operation
                ALUOp <= OP_XOR;
                RegWSel <= IR(8 downto 4);
                RegSelB <= IR(9) & IR(3 downto 0);
                BitMask <= MASK_EOR;
            end if;

            if  std_match(IR, OpLSR) then
                -- enable shifter/rotator operation
                ALUSel <= ShiftEn;
                -- select LSR operation
                ALUOp <= OP_LSR;
                RegWSel <= IR(8 downto 4);
                BitMask <= MASK_SHIFT;
            end if;

            if  std_match(IR, OpASR) then
                -- enable shifter/rotator operation
                ALUSel <= ShiftEn;
                -- select LSR operation
                ALUOp <= OP_ASR;
                RegWSel <= IR(8 downto 4);
                BitMask <= MASK_SHIFT;
            end if;

            if  std_match(IR, OpROR) then
                -- enable shifter/rotator operation
                ALUSel <= ShiftEn;
                -- select LSR operation
                ALUOp <= OP_ROR;
                -- ROR op uses carry bit from last operation
                ALUOp(carryBit) <= SReg(0);
                RegWSel <= IR(8 downto 4);
                BitMask <= MASK_SHIFT;
            end if;

            if  std_match(IR, OpBCLR) or std_match(IR, OpBSET) then
                -- status register source is Control Unit
                SRegLd <= LdSRCtrlU;
                -- set or reset all status register outputs
                SRegOut <= (others => not IR(7));
                -- specific bit to be cleared/set uses bitmask
                --  clear bitmask
                bitmask <= (others => '0');
                --  then set proper bit high in bitmask
                bitmask(conv_integer(IR(6 downto 4))) <= '1';
            end if;

            if  std_match(IR, OpBLD) or std_match(IR, OpBST) then
                ALUSel <= PassThruEn;
                RegWSel <= IR(8 downto 4);
                -- clear bitmask
                BitMask <= (others => '0');
                -- store/loads T bit
                BitMask(T_SREG) <= IR(T_IR);
            end if;

            if std_match(IR, OpSWAP) then
                -- register array handles nibble swapping
                LoadReg <= LoadSwap;
                LoadIn <= LdRegA;
                RegWSel <= IR(8 downto 4);
                BitMask <= MASK_NONE;
            end if;

            if std_match(IR, OpIN) or std_match(IR, OpOUT) then
                -- not done
                RegWSel <= IR(8 downto 4);
                RegWEn     <= IR(11);
                IORegInEn  <= not IR(11);
                IORegOutEn <= IR(11);
                LoadIn <= LdIO;
            end if;

        end if;
    end process decoder;

    -- load enable signal telling when to fetch next instruction
    load <= '1' when cycle = cycle_num - 1 else
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














