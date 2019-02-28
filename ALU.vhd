

----------------------------------------------------------------------------
--
--
-- Arithmetic Logic Unit
--
-- ALU implementation for the AVR CPU, responsible for arithmetic and logic
-- operations, including boolean operations, shifts and rotates, bit functions,
-- addition, subtraction, and comparison. The operands may be registers or
-- immediate values.
-- The ALU consists of a functional block for logical operations,
-- an adder/subtracter, and a shifter/rotator. It takes several control
-- signals and flags from the control unit as inputs, along with the operands A
-- and B from the register array, or from an immediate value encoded in the
-- instruction. It outputs the computed result and computed flags.
--
-- Inputs:
--
-- ALUASOp      - 3 bit ALU adder/subber operation signals
-- ALUSROp      - 2 bit ALU shifter/rotator operation signals
-- ALUFop       - 4 bit ALU F Block operation signals
-- ALUCNOp      - 2 bit ALU com/neg operation signals
-- ALUSel       - 3 bit ALU output select
-- BitMask      - 8 bit mask for writing to status flags
-- CPC          - 1 bit control for cpc command, to set zero flag appropriately
-- RegA         - 8 bit operand A
-- RegA         - 8 bit operand B, or immediate value
-- StatusIn     - 8 bit status flag input
--
-- Outputs:
--
-- RegOut       - 8 bit output result
-- StatusOut    - 8 bit status flag output
-- SBFlag       - 1 bit skip bit flag output for SBRC/SBRS
-- ZeroFlag     - 1 bit zero flag output for CPSE
--
-- Revision History:
-- 01/24/2019   Sophia Liu      Initial revision
-- 01/28/2019   Sophia Liu      Initial architecture revision
-- 01/31/2019   Sundar Pandian  Added support for BST, BLD
-- 02/01/2019   Sophia Liu      Updates for CU support
-- 02/06/2019   Sundar Pandian  Changed structure so ALU does it all
--
----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

use work.opcodes.all;

use work.constants.all;
use work.ALUConstants.all;

entity ALU is
    port(
        -- from CU
        ALUASOp     : in    ALU_ADDSUB;     -- add/sub operation control signals
        ALUSROp     : in    ALU_SR;         -- shift/rotate operation control signals
        ALUFOp      : in    ALU_FOPS;       -- F-block operation control signals
        ALUCNOp     : in    ALU_COMNEG;     -- COM/NEG operation control signals
        ALUSel      : in    ALU_SELECTS;    -- operation select

        BitMask     : in    std_logic_vector(REGSIZE-1 downto 0);
        CPC         : in    std_logic; -- control for cpc command, to set zero flag appropriately

        -- from Regs
        RegA        : in    std_logic_vector(REGSIZE-1 downto 0); -- operand A
        RegB        : in    std_logic_vector(REGSIZE-1 downto 0); -- operand B, or immediate
        StatusIn    : in    std_logic_vector(REGSIZE-1 downto 0);

        RegOut      : out   std_logic_vector(REGSIZE-1 downto 0); -- output result
        StatusOut   : out   std_logic_vector(REGSIZE-1 downto 0); -- status register output
        SBFlag      : out   std_logic;                            -- skip bit flag output for SBRC/SBRS
        ZeroFlag    : out   std_logic                             -- zero flag output for CPSE
    );
end ALU;

architecture behavioral of ALU is

-- internal signals
signal AdderOut     : std_logic_vector(REGSIZE-1 downto 0); -- adder/subtracter output
signal CarryOut     : std_logic_vector(REGSIZE-1 downto 0); -- carry for adder/subtracter

-- mult signals
type MULARR is array (natural range <>) of std_logic_vector(REGSIZE-1 downto 0);
signal CarryOutMul  : MULARR(REGSIZE-1 downto 0); -- carry for mul
signal AOpMul       : MULARR(REGSIZE-1 downto 0); -- internal signals for mul
signal BOpMul       : MULARR(REGSIZE-2 downto 0); -- internal signals for mul

--signal ASCout       : std_logic;
signal Bin          : std_logic_vector(REGSIZE-1 downto 0); -- B Input to the adder/subber

signal Fout         : std_logic_vector(REGSIZE-1 downto 0); -- f block output

signal SRout        : std_logic_vector(REGSIZE-1 downto 0); -- shifter/rotator block output

signal Bitout       : std_logic_vector(REGSIZE-1 downto 0); -- bit set block output

signal SwapRegOut   : std_logic_vector(REGSIZE-1 downto 0); -- swap block output

signal MulRegOut    : std_logic_vector(REGSIZE*2-1 downto 0); -- MUL block output

signal RegBuff      : std_logic_vector(REGSIZE-1 downto 0); -- buffer for output result

-- internal status signals
signal VFlag        : std_logic; -- signed overflow status flag
signal NFlag        : std_logic;
signal CFlag        : std_logic;

signal CIn          : std_logic; -- carry in from adder sel

signal comnegR      : std_logic_vector(REGSIZE-1 downto 0);

signal nCarryFlag   : std_logic;
---- component declarations

component Mux8to1 is
    port(
        S0          :  in      std_logic;  -- mux sel(0)
        S1          :  in      std_logic;  -- mux sel(1)
        S2          :  in      std_logic;  -- mux sel(2)
        SIn0        :  in      std_logic;  -- mux inputs
        SIn1        :  in      std_logic;  -- mux inputs
        SIn2        :  in      std_logic;  -- mux inputs
        SIn3        :  in      std_logic;  -- mux inputs
        SIn4        :  in      std_logic;  -- mux inputs
        SIn5        :  in      std_logic;  -- mux inputs
        SIn6        :  in      std_logic;  -- mux inputs
        SIn7        :  in      std_logic;  -- mux inputs
        SOut        :  out     std_logic   -- mux output
      );
end component;

component Mux4to1 is
    port(
        S0          :  in      std_logic;  -- mux sel(0)
        S1          :  in      std_logic;  -- mux sel(1)
        SIn0        :  in      std_logic;  -- mux inputs
        SIn1        :  in      std_logic;  -- mux inputs
        SIn2        :  in      std_logic;  -- mux inputs
        SIn3        :  in      std_logic;  -- mux inputs
        SOut        :  out     std_logic   -- mux output
      );
end component;

component fullAdder is
    port(
        A           :  in      std_logic;  -- adder input
        B           :  in      std_logic;  -- adder input
        Cin         :  in      std_logic;  -- carry in value
        Cout        :  out     std_logic;  -- carry out value
        Sum         :  out     std_logic   -- sum of A, B with carry in
      );
end component;

begin
    -- fblock
    GenFBlock:  for i in REGSIZE-1 downto 0 generate
      FBlocki: Mux4to1
        port map(
            S0          => RegB(i),
            S1          => RegA(i),
            SIn0        => ALUFOp(0),
            SIn1        => ALUFOp(1),
            SIn2        => ALUFOp(2),
            SIn3        => ALUFOp(3),
            SOut        => FOut(i)
      );
      end generate GenFBlock;

    -- clear or set A, for use with COM or NEG
    -- TODO explain?
    GenAClr: for i in REGSIZE-1 downto 0 generate
        comnegR(i) <= (RegA(i) and ALUCNOp(OP_CN_AND)) or ALUCNOp(OP_CN_OR);
    end generate GenAClr;

    nCarryFlag <= not StatusIn(0);

    -- adder/subtracter carry in MUX
    adderCarry: Mux4to1
        port map(
            S0          => ALUASOp(CARRY_S0),
            S1          => ALUASOp(CARRY_S1),
            SIn0        => '0',
            SIn1        => '1',
            SIn2        => StatusIn(0),
            SIn3        => nCarryFlag,
            SOut        => CIn
      );

    -- flip bits in B if subtracting
    SubXOR: for i in REGSIZE-1 downto 0 generate
        Bin(i) <= FOut(i) xor ALUASOp(SUBFLAG);
    end generate SubXOR;

    adder0_ALU: fullAdder --TODO check
    port map(
        A           => comnegR(0),
        B           => Bin(0),
        Cin         => CIn,
        Cout        => Carryout(0),
        Sum         => AdderOut(0)
  );
  -- other bits
  GenAdder_ALU:  for i in 1 to REGSIZE - 1 generate
  adderi_ALU: fullAdder
    port map(
        A           => comnegR(i),
        B           => Bin(i),
        Cin         => CarryOut(i-1),
        Cout        => Carryout(i),
        Sum         => AdderOut(i)
  );
  end generate GenAdder_ALU;

  -- zero flag outputted to Ctrl Unit for CPSE
  ZeroFlag <= '1' when RegBuff = ZERO8 else
              '0';

    -- shifter/rotator
    -- assign middle and low bits
    SROut(REGSIZE - 2 downto 0) <= RegA(REGSIZE - 1 downto 1);
    -- assign high bit
    SRMux: Mux4to1
        port map(
            S0          => ALUSROp(0),
            S1          => ALUSROp(1),
            SIn0        => '0',             -- LSR high bit = 0
            SIn1        => RegA(REGSIZE-1), -- ASR high bit constant
            SIn2        => StatusIn(0),     -- ROR high bit = carry in
            SIn3        => 'X',
            SOut        => SROut(REGSIZE-1)
      );

    -- transfer bit loading
    -- RegB holds Immediate value TODO
    BIT_OP : for i in REGSIZE-1 downto 0 generate
        Bitout(i)   <=  StatusIn(6) when i = to_integer(unsigned(RegB(2 downto 0))) else
                        RegA(i);
    end generate;

    -- Skip bit flag outputted to Ctrl Unit for CPSE
    SBFlag <= RegA(to_integer(unsigned(RegB(2 downto 0))));

    -- SWAP block high NIBBLE
    -- switches high and low nibble of A input
    SWAP_OP_HI : for i in REGSIZE-1 downto NIBBLE generate
        SwapRegOut(i) <= RegA(i-NIBBLE);
    end generate;

    -- SWAP block low NIBBLE
    -- switches high and low nibble of A input
    SWAP_OP_LO : for i in NIBBLE-1 downto 0 generate
        SwapRegOut(i) <= RegA(i+NIBBLE);
    end generate;

    -- 8x8 Array Multiplier
    -- Array setup
    Mul_ALU:  for i in 1 to REGSIZE - 1 generate
        AOpMul(0)(i) <= RegA(i) and RegB(0); -- first row array input
    end generate;
    MulRegOut(0) <= RegA(0) and RegB(0); -- ab0 = a0 and b0
    -- Multiplier sum array
    -- first array row
    Mula0_ALU:  for ai in 1 to REGSIZE - 1 generate
            BOpMul(0)(ai) <= RegA(ai-1) and RegB(1);
            MulAdd0_ALU: fullAdder
            port map(
                A           => AOpMul(0)(ai),
                B           => BOpMul(0)(ai),
                Cin         => '0',
                Cout        => CarryOutMul(0)(ai-1),
                Sum         => AOpMul(1)(ai-1)
            );
    end generate;
    MulRegOut(1) <= AOpMul(1)(0);
    AOpMul(1)(REGSIZE-1) <= RegA(REGSIZE-1) and RegB(1);
    -- middle array rows
    Mulb_ALU:  for bi in 1 to REGSIZE - 2 generate
        Mula_ALU:  for ai in 1 to REGSIZE - 1 generate
            BOpMul(bi)(ai) <= RegA(ai-1) and RegB(bi+1);
            MulAdd_ALU: fullAdder
            port map(
                A           => AOpMul(bi)(ai),
                B           => BOpMul(bi)(ai),
                Cin         => CarryOutMul(bi-1)(ai-1),
                Cout        => CarryOutMul(bi)(ai-1),
                Sum         => AOpMul(bi+1)(ai-1)
            );
        end generate;
        MulRegOut(bi+1) <= AOpMul(bi+1)(0);
        AOpMul(bi+1)(REGSIZE-1) <= RegA(REGSIZE-1) and RegB(bi+1);
    end generate;
    -- last array row
    Mula1_ALU:  for ai in 1 to REGSIZE - 1 generate
        CarryOutMul(REGSIZE-1)(0) <= '0';
        MulAdd1_ALU: fullAdder
        port map(
            A           => AOpMul(REGSIZE-1)(ai),
            B           => CarryOutMul(REGSIZE-1)(ai-1),
            Cin         => CarryOutMul(REGSIZE-2)(ai-1),
            Cout        => CarryOutMul(REGSIZE-1)(ai),
            Sum         => MulRegOut(ai+7)
        );
    end generate;
    MulRegOut(15) <= CarryOutMul(REGSIZE-1)(REGSIZE-1); -- msb is last carry bit

    RegBuff <=  AdderOut    when ALUSel = ADDSUBOUT else
                FOut        when ALUSel = FBLOCKOUT else
                SROut       when ALUSel = SHIFTOUT  else
                SwapRegOut  when ALUSel = SWAPOUT   else
                MulRegOut(REGSIZE-1 downto 0)   when ALUSel = MULOUTL    else
                MulRegOut(REGSIZE*2-1 downto REGSIZE)   when ALUSel = MULOUTH    else
                BitOut      when ALUSel = BOUT else
                "XXXXXXXX";


    RegOut <= RegBuff;

    -- Status Register logic

    -- interrupt bit
    StatusOut(7) <= StatusIn(7) when BitMask(7) = '0' else
                    '1' when ALUSel = BSET else
                    '0' when ALUSel = BCLR else
                    not StatusIn(7); -- flips between setting and resetting global interrupt
                                     --  enable between CALLI and RETI

    -- transfer bit
    StatusOut(6) <= StatusIn(6) when BitMask(6) = '0' else
                    RegA(to_integer(unsigned(RegB(2 downto 0)))) when ALUSel = BOUT else
                    '1' when ALUSel = BSET else
                    '0' when ALUSel = BCLR else
                    '0'; -- update if transfer bit is set or cleared

    -- half carry
    StatusOut(5) <= StatusIn(5) when BitMask(5) = '0' else
                    '1' when ALUSel = BSET else
                    '0' when ALUSel = BCLR else
                    CarryOut(HALFCARRYBIT) when ALUASOp(SUBFLAG) = OP_ADD else
                    not CarryOut(HALFCARRYBIT);     -- carry flag opposite when subtracting

     -- corrected signed
    StatusOut(4) <= StatusIn(4) when BitMask(4) = '0' else
                    '1' when ALUSel = BSET else
                    '0' when ALUSel = BCLR else
                    NFlag xor VFlag;

    -- signed overflow
    VFlag <= StatusIn(3) when BitMask(3) = '0' else
             '1' when ALUSel = BSET else
             '0' when ALUSel = BCLR else
             '1' when (ALUSEL = ADDSUBOUT and CarryOut(REGSIZE-1) /= CarryOut(REGSIZE-2)) else -- 1 if signed overflow
             '0' when (ALUSEL = ADDSUBOUT or  ALUSEL = FBLOCKOUT) else -- 0 if no overflow or logical op
             NFlag xor CFlag;  --N xor C for shift operations
    StatusOut(3) <= VFlag;

    -- negative
    NFlag <= StatusIn(2) when BitMask(2) = '0' else
             '1' when ALUSel = BSET else
             '0' when ALUSel = BCLR else
             RegBuff(REGSIZE-1); -- set based on sign bit
    StatusOut(2) <= NFlag;

    -- zero flag
    StatusOut(1) <= StatusIn(1) when BitMask(1) = '0' else
                    ('1' and StatusIn(1)) when RegBuff = ZERO8 and ALUFOp = FOP_ZERO and ALUASOp(CARRY_S1 downto CARRY_S0) /= SET_CARRY else
                    '1' when ALUSel = BSET else
                    '0' when ALUSel = BCLR else
                    '0' when CPC = '1' and StatusIn(1) = '0' else -- and zero flag if performing cpc
                    '1' when RegBuff = ZERO8 else -- compare with 0
                    '0';

    -- carry
    CFlag <= StatusIn(0) when BitMask(0) = '0' else
                    '1' when ALUSel = BSET else
                    '0' when ALUSel = BCLR else
                    '1' when ALUCNOp = ALU_COM else
                    CarryOut(REGSIZE-1) when ALUSel = ADDSUBOUT and ALUASOp(SUBFLAG) = OP_ADD else
                    not CarryOut(REGSIZE-1) when ALUSel = ADDSUBOUT else     -- carry flag opposite when subtracting
                    '1' when ALUSel = FBLOCKOUT else -- set for logical operations
                    MulRegOut(REGSIZE*2-1) when ALUSel = MULOUTH else -- carry out for multiplication 
                    RegA(0); -- when ALUSel = SHIFTEN;
    StatusOut(0) <= CFlag;
end behavioral;
