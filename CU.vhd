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
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

library opcodes; 
use opcodes.opcodes.all; 

library CPUconst;
use CPUconst.constants.all;

entity CU is
    port(
        ProgDB  : in std_logic_vector(15 downto 0); -- program memory data bus
        SReg    : in std_logic_vector(7 downto 0);  -- status flags
        
        -- to registers
        RegWEn      : out std_logic; -- register write enable 
        RegWSel     : out std_logic_vector(4 downto 0); -- register write select
        RegSelA     : out std_logic_vector(4 downto 0); -- register A select
        RegSelB     : out std_logic_vector(4 downto 0); -- register B select
        RegDataSel  : out std_logic_vector(3 downto 0);

        -- to ALU and SReg
        ALUOp   : out std_logic_vector(4 downto 0); -- operation control signals 
        ALUSel  : out std_logic_vector(2 downto 0); -- operation select 
        bitmask : out std_logic_vector(7 downto 0); -- mask for writing to flags
        
        -- I/O
        IORegInEn   : out   std_logic;                      -- 
        IORegOutEn  : out   std_logic;                      --
        SRegOut     : out   std_logic_vector(7 downto 0); 
        K           : out std_logic_vector(7 downto 0); -- immediate value K
        
        
        -- Program memory access
        ProgAddr: out std_logic_vector(15 downto 0); -- address source for program memory unit
        ProgLoad: out std_logic;                    -- load select for PC
        ProgAddrSel : in std_logic_vector(1 downto 0);  -- program address source select 
        
        -- Data memory access       
        DataAddrSel : out std_logic_vector(1 downto 0);  -- data address source select
        DataOffsetSel : out std_logic_vector(1 downto 0);-- data address offset source select 
        PreSel  : out std_logic; -- data pre/post address select 
        DataRd  : out std_logic; -- indicates data memory is being read
        DataWr  : out std_logic; -- indicates data memory is being written
        DataAddr: out std_logic_vector(15 downto 0); -- address source for data memory unit
        AddrOffset: out std_logic_vector(5 downto 0); -- address offset for data memory unit 
        
        -- Stack
        StackEn     : out std_logic; -- stack enable signal 
        StackPush   : out std_logic; -- stack push/pop control
        Reset       : out std_logic -- active low reset signal
    );

end CU;

--
--  CU Architecture
--

-- TODO: where is the K enable pin going?? 
-- borrow flag still active low with SBCI?? 
-- mux in K for both first or second operand
-- if Ken active, RegA(8) <= '1';
-- if Ken active, RegW(8) <= '1';

architecture RISC of CU is

    -- FSM States
    type states is (
        Done,
        Count
    );

    signal FSMState : states := Done;

    signal CLK : std_logic;
    
    signal load         :   std_logic;

    signal cycle_num    :   std_logic_vector(1 downto 0);
    signal cycle        :   std_logic_vector(1 downto 0);

    signal IR           :   std_logic_vector(15 downto 0);

begin

    Decoder : process (CLK)
    begin
        if (rising_edge(CLK)) then
            -- sets cycle number for op_codes 
            -- defaults to 1
            cycle_num <= "01";
            if (std_match(IR, OpADIW) or 
                std_match(IR, OpADIW) or 
                std_match(IR, OpADIW) or 
                std_match(IR, OpADIW)) then
                    cycle_num <= "10";
            end if;

            -- register default values
            K <= IR(11 downto 8) & IR(3 downto 0);
            RegSelA <= IR(8 downto 4);
            RegWEn  <= '1';

            -- 3 MSBs of IR for AVR all use adder/subber
            --  block for ALU ops except for those that 
            --  are MULS ops. These are not supported so:

            -- considering single byte adder/subber ops
            if  std_match(IR, "000-------------") then
                -- enable Adder/Subber operation
                ALUSel <= AddSubEn;
                -- carry/nborrow bit mapped in IR
                ALUOp(carryBit) <= IR(12);
                -- subber flag mapped in IR as function of 2 bits
                ALUOp(subFlag)  <= IR(11) xor IR(10);
                -- CP and CPC doesn't rewrite register
                RegWEn <= IR(11);

                RegSelB <= IR(9) & IR(3..0);
                RegWSel <= IR(8..4);
            end if;

            -- considering word adder/subber ops
            if  std_match(IR, OpADIW) or std_match(IR, OpSBIW) then
                -- enable Adder/Subber operation
                ALUSel <= AddSubEn;
                -- subFlag mapped in IR
                ALUOp(subFlag)  <= IR(8);
                -- mapping to immediate value in IR
                K <= "00" & IR(7 downto 6) & IR(3 downto 0);

                -- value in IR is offset added to register 24
                --  possible operands include {24, 26, 28, 30}
                --  low byte operation uses above bytes while high byte
                --    operation uses the next highest byte

                -- first do low byte addition
                if cycle = "00" then
                    -- carry in mapped in IR
                    --  clear active-hi carry for add
                    --  clear active-lo borrow for sub
                    -- maps same as to subFlag
                    ALUOp(carryBit) <= IR(8);
                    RegSelA <= "11" & IR(5 downto 4) & '0';
                    RegWSel <= "11" & IR(5 downto 4) & '0';
                elsif cycle = "01"
                    -- carry out from low byte carries in to high byte add
                    ALUOp(carryBit) <= SReg(0);
                    RegSelA <= "11" & IR(5 downto 4) & '1';
                    RegWSel <= "11" & IR(5 downto 4) & '1';
                end if;
            end if;

            -- considering word multiply op
            if  std_match(IR, OpMUL) then
                -- enable MUL operation
                ALUSel <= MulEn;

                -- output of MUL op is saved in R1:R0
                --  low byte operation uses above bytes while high byte
                --    operation uses the next highest byte

                -- first do low byte multiply
                if cycle = "00" then
                    RegSelB <= IR(9) & IR(3..0);
                    RegWSel <= "00000";
                elsif cycle = "01"
                    RegSelB <= IR(9) & IR(3..0);
                    RegWSel <= "00001";
                end if;
            end if;

            -- considering immediate subber operations
            if  std_match(IR, OpSUBI) or std_match(IR, OpSBCI) or std_match(IR, OpCPI) then
                -- enable Adder/Subber operation
                ALUSel <= '1';
                -- carry/nborrow bit mapped in IR
                ALUOp(carryBit) <= IR(12);
                -- subbing so subFlag active
                ALUOp(subFlag)  <= '1';
                -- CPI doesn't rewrite register
                RegWEn <= IR(14);

                RegSelA <= '1' & IR(7 downto 4);
                RegWSel <= '1' & IR(7 downto 4);
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
                RegWSel <= IR(8 downto 4);
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
                K <= (others => IR(0));
                -- TODO: mux K into second operand
                RegWSel <= IR(8 downto 4);
            end if;

            if  std_match(IR, OpAND) or std_match(IR, OpANDI) then
                -- enable F Block Operation
                ALUSel <= FBlockEn;
                -- select AND operation
                ALUOp <= OP_AND;
                -- TODO: K_en <= IR(14);
                RegWSel <= IR(8 downto 4);
                if std_match(IR, OpANDI) then
                    RegSelA(8) <= '1';
                    RegWSel(8) <= '1';
                end if;
                RegSelB <= IR(9) & IR(3..0);
            end if;

            if  std_match(IR, OpOR) or std_match(IR, OpORI) then
                -- enable F Block Operation
                ALUSel <= FBlockEn;
                -- select OR operation
                ALUOp <= OP_OR;
                -- TODO: K_en <= IR(14);
                RegWSel <= IR(8 downto 4);
                if std_match(IR, OpORI) then
                    RegSelA(8) <= '1';
                    RegWSel(8) <= '1';
                end if;
                RegSelB <= IR(9) & IR(3..0);
            end if;

            if  std_match(IR, OpEOR) then
                -- enable F Block Operation
                ALUSel <= FBlockEn;
                -- select AND operation
                ALUOp <= OP_XOR;
                -- TODO: K_en <= IR(14);
                RegWSel <= IR(8 downto 4);
                RegSelB <= IR(9) & IR(3..0);
            end if;

            if  std_match(IR, OpLSR) then
                -- enable shifter/rotator operation
                ALUSel <= ShiftEn;
                -- select LSR operation
                ALUOp <= OP_LSR;
                RegWSel <= IR(8 downto 4);
            end if;

            if  std_match(IR, OpASR) then
                -- enable shifter/rotator operation
                ALUSel <= ShiftEn;
                -- select LSR operation
                ALUOp <= OP_ASR;
                RegWSel <= IR(8 downto 4);
            end if;

            if  std_match(IR, OpROR) then
                -- enable shifter/rotator operation
                ALUSel <= ShiftEn;
                -- select LSR operation
                ALUOp <= OP_ROR;
                RegWSel <= IR(8 downto 4);
            end if;

            if  std_match(IR, OpBCLR) or std_match(IR, OpBSET) then
                SRegOut <= (others => not IR(7));
                bitmask <= (conv_integer(IR(6 downto 4)) => '1',
                            others                       => '0');
            end if;

            if  std_match(IR, OpBLD) or std_match(IR, OpBST) then
                ALUSel <= PassThruEn;
                RegDataSel <= 
                RegWSel <= IR(8 downto 4);
                (conv_integer(IR(6 downto 4)))
            end if;

            if std_match(IR, OpSWAP) then
                RegDataSel <= RData_SWAP;
                RegWSel <= IR(8 downto 4);
            end if;
    
        end if;
    end process Decoder;


    load <= '1' when cycle = cycle_num else
            '0';

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

    ---- handles FSM transitions
    --FSM_trans : process (CLK)
    --begin
    --    if (rising_edge(CLK)) then
    --        case FSMState is
    --            when Done =>
    --                if cycle_num /= cycle then
    --                    FSMState <= Count;
    --                end if;
    --            when Count =>
    --                if cycle_num = cycle then
    --                    FSMState <= Done;
    --                end if;
    --        end case;
    --    end if;
    --end process FSM_trans;

    ---- handles FSM actions
    --FSM_actions : process (CLK)
    --begin
    --    if (rising_edge(CLK)) then
    --        case FSMState is
    --            when Done =>
    --                load <= '1';
    --                cycle = "00";
    --            when Count =>
    --                load <= '0';
    --                cycle <= cycle + 1;
    --        end case;
    --    end if;
    --end process FSM_actions;

end RISC;














