package mini

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters  //引入依赖

object Const {
  val PC_START = 0x200//起始地址
  val PC_EVEC  = 0x100//EVEC是一个CSR寄存器
}
//implicit:代码压缩，减少模块代码；此处是隐式参数作用
//DatapathIO继承CoreBundle
//CoreBundle：抽象类，接收DatapathIO传来的隐式参数
class DatapathIO(implicit p: Parameters) extends CoreBundle()(p) {//IO数据通路
  val host = new HostIO //主机IO
  val icache = Flipped(new CacheIO) //Flipped将I/O互换，icache缓存指令
  val dcache = Flipped(new CacheIO) //dcache缓存数据
  val ctrl = Flipped(new ControlSignals) //控制信号
}
//Datapath继承CoreParams模组，CoreParams是一个特质，其中有未实现的抽象成员，即P
class Datapath(implicit val p: Parameters) extends Module with CoreParams {//数据通路
  val io       = IO(new DatapathIO) //IO数据通路
  val csr      = Module(new CSR) //寄存器模组
  val regFile  = Module(new RegFile) //寄存器文件模组
  val alu      = p(BuildALU)(p) //ALU算术逻辑单元
  val immGen   = p(BuildImmGen)(p) //定义immGen（立即数生成器）
  val brCond   = p(BuildBrCond)(p) //定义brCond（分支跳转）

  import Control._

  //decode stage
  val id_pc       = Reg(UInt(xlen.W))
  val id_inst     = RegInit(Instructions.NOP)

  //execute stage
  val exe_pc       = Reg(UInt(xlen.W))
  val exe_inst     = RegInit(Instructions.NOP)
  val exe_immout   = Reg(UInt(xlen.W))
  val exe_immsel   = Reg(UInt(3.W))
  val a_sel        = Reg(UInt(1.W))
  val b_sel        = Reg(UInt(1.W))
  val alu_op       = Reg(UInt(4.W))
  val exe_br_type  = Reg(UInt(3.W))
  val exe_csr_in   = Reg(UInt(xlen.W))
  val exe_st_type  = Reg(UInt(2.W))
  val exe_ld_type  = Reg(UInt(3.W))
  val exe_wb_sel   = Reg(UInt(2.W))
  val exe_wb_en    = Reg(Bool())
  val exe_csr_cmd  = Reg(UInt(3.W))
  val exe_illegal  = Reg(Bool())
  val exe_pc_check = Reg(Bool())
  val exe_rs1_addr = Reg(UInt(5.W))
  val exe_rs2_addr = Reg(UInt(5.W))
  val exe_rdata1   = Reg(UInt(xlen.W))
  val exe_rdata2   = Reg(UInt(xlen.W))
  val exe_rs1      = Reg(UInt(xlen.W))
  val exe_rs2      = Reg(UInt(xlen.W))


  //memory stage
  val mem_pc       = Reg(UInt(xlen.W))
  val mem_inst     = RegInit(Instructions.NOP)
  val mem_rs1      = Reg(UInt(xlen.W))
  val mem_rs2      = Reg(UInt(xlen.W))
  val mem_immout   = Reg(UInt(xlen.W))
  val mem_immsel   = Reg(UInt(3.W))
  val mem_alu      = Reg(UInt(xlen.W))
  val mem_sum      = Reg(UInt(xlen.W))
  val mem_csr_in   = Reg(UInt(xlen.W))
  val mem_st_type  = Reg(UInt(2.W))
  val mem_ld_type  = Reg(UInt(3.W))
  val mem_wb_sel   = Reg(UInt(2.W))
  val mem_wb_en    = Reg(Bool())
  val mem_csr_cmd  = Reg(UInt(3.W))
  val mem_illegal  = Reg(Bool())
  val mem_pc_check = Reg(Bool())

  //writeback stage
  val wb_pc        = Reg(UInt(xlen.W))
  val wb_inst      = RegInit(Instructions.NOP)
  val wb_alu       = Reg(UInt(xlen.W))
  val wb_csr_in    = Reg(UInt(xlen.W))
  val wb_st_type   = Reg(UInt(2.W))
  val wb_ld_type   = Reg(UInt(3.W))
  val wb_wb_sel    = Reg(UInt(2.W))
  val wb_en        = Reg(Bool())
  val wb_csr_cmd   = Reg(UInt(3.W))
  val wb_illegal   = Reg(Bool())
  val wb_pc_check  = Reg(Bool())
  val wb_data      = Reg(UInt(xlen.W))

  //vaR regWrite     = Reg(UInt(xlen.W))


  val br_addr           = Reg(UInt(xlen.W))
  val flush             = Reg(Bool())
  val exe_flush         = Reg(Bool())
  val mem_flush         = Reg(Bool())

  //装载指令优化
  val ld_flag           = Reg(Bool())
  //val rs1_ld_hazard     = RegInit(false.B)
  //val rs2_ld_hazard     = RegInit(false.B)
  //val exe_rs1_ld_hazard  = RegInit(false.B)
  //val exe_rs2_ld_hazard  = RegInit(false.B)


  //fetch stage--------------------------------

  val started = RegNext(reset.toBool)
  val stall   = !io.icache.resp.valid || !io.dcache.resp.valid
  val if_pc   = RegInit(Const.PC_START.U(xlen.W) - 4.U(xlen.W))


  val if_npc  = Mux(stall, if_pc,
    Mux(csr.io.expt, csr.io.evec,
      Mux(io.ctrl.pc_sel === PC_EPC,  csr.io.epc,
        Mux(io.ctrl.pc_sel === PC_ALU || brCond.io.taken, alu.io.sum >> 1.U << 1.U,
          Mux(io.ctrl.pc_sel === PC_0, if_pc,
            Mux(exe_flush === true.B, br_addr, if_pc + 4.U))))))


  val if_inst = Mux(started || io.ctrl.inst_kill || brCond.io.taken || csr.io.expt , Instructions.NOP, io.icache.resp.bits.data)

  if(p(Trace)){
    printf("INST: %d\n",io.icache.resp.bits.data)
  }

  if_pc := if_npc
  io.icache.req.bits.addr := if_npc
  io.icache.req.bits.data := 0.U
  io.icache.req.bits.mask := 0.U
  io.icache.req.valid     := !stall
  io.icache.abort         := false.B


  //pipelining
  when(!stall)
  {
    id_pc   := if_pc
    id_inst := if_inst
  }


  //decode stage--------------------------------
  io.ctrl.inst  := id_inst
  val rd_addr = id_inst(11,7)
  val rs1_addr= id_inst(19,15)
  val rs2_addr= id_inst(24,20)

  if(p(Trace)){
      printf("Decode stage\n")
      printf("rs1: REG[%d]\n",rs1_addr)
      printf("rs2: REG[%d]\n",rs2_addr)
      printf("rd: REG[%d]\n",rd_addr)
      printf("\n")
  }

  regFile.io.raddr1 := rs1_addr
  regFile.io.raddr2 := rs2_addr

  immGen.io.inst := id_inst
  immGen.io.sel  := io.ctrl.imm_sel



  //bypass
  val exe_rd_addr    = exe_inst(11,7)
  val mem_rd_addr    = mem_inst(11,7)
  val wb_rd_addr     = wb_inst(11,7)


  //load指令优化  冒险检测单元
  ld_flag := Mux(exe_ld_type === LD_XXX, false.B, true.B)
  val rs1_ld_hazard = ld_flag && (rs1_addr === mem_rd_addr)
  val rs2_ld_hazard = ld_flag && (rs2_addr === mem_rd_addr)

  val em_rs1hazard  = exe_wb_en && rs1_addr.orR && (rs1_addr === exe_rd_addr)
  val em_rs2hazard  = exe_wb_en && rs2_addr.orR && (rs2_addr === exe_rd_addr)

  val mw_rs1hazard  = mem_wb_en && rs1_addr.orR && (rs1_addr === mem_rd_addr)
  val mw_rs2hazard  = mem_wb_en && rs2_addr.orR && (rs2_addr === mem_rd_addr)

  val rs1 = Mux(exe_wb_sel  === WB_ALU && em_rs1hazard, alu.io.out,
    Mux(mem_wb_sel === WB_ALU && mw_rs1hazard, mem_alu,
      Mux(rs1_ld_hazard,io.dcache.resp.bits.data,regFile.io.rdata1)))
  val rs2 = Mux(exe_wb_sel  === WB_ALU && em_rs2hazard, alu.io.out,
    Mux(mem_wb_sel === WB_ALU && mw_rs2hazard, mem_alu,
      Mux(rs2_ld_hazard,io.dcache.resp.bits.data,regFile.io.rdata2)))




  //分支优化模块
  //分支跳转优化
  val b_num  = io.ctrl.b_type  //获取具体操作
  flush := Mux(b_num === 1.U && rs1 === rs2, true.B,
    Mux(b_num === 2.U && rs1 =/= rs2, true.B,
      Mux(b_num === 3.U && rs1 < rs2,   true.B,
        Mux(b_num === 4.U && rs1 > rs2,   true.B, false.B))))
  br_addr := immGen.io.out + exe_pc
  when(flush)
  {
    id_inst  := Instructions.NOP
  }

  //pipelining
  when(reset.toBool || !stall && csr.io.expt) {//&&优先级高于||，此处为如果获取到重置信息或（cache完毕但是寄存器未准备完毕），则执行
    exe_st_type   := 0.U//以下都是重置操作
    exe_ld_type   := 0.U
    exe_wb_en     := false.B
    exe_csr_cmd   := 0.U
    exe_illegal   := false.B
    exe_pc_check  := false.B
  }.elsewhen(!stall && !csr.io.expt) {//若不重置，寄存器和cache都正常，执行下列操作
    exe_rs1               := rs1
    exe_rs2               := rs2
    exe_pc                := id_pc
    exe_inst              := id_inst
    exe_immout            := immGen.io.out
    exe_immsel            := io.ctrl.imm_sel
    a_sel                 := io.ctrl.A_sel
    b_sel                 := io.ctrl.B_sel
    alu_op                := io.ctrl.alu_op
    exe_br_type           := io.ctrl.br_type
    exe_csr_in            := Mux(io.ctrl.imm_sel === IMM_Z, exe_immout, rs1)
    exe_st_type           := io.ctrl.st_type
    exe_ld_type           := io.ctrl.ld_type
    exe_wb_sel            := io.ctrl.wb_sel
    exe_wb_en             := io.ctrl.wb_en
    exe_csr_cmd           := io.ctrl.csr_cmd
    exe_illegal           := io.ctrl.illegal
    exe_pc_check          := io.ctrl.pc_sel === PC_ALU
    exe_rs1_addr          := rs1_addr
    exe_rs2_addr          := rs2_addr
    exe_rdata1            := regFile.io.rdata1
    exe_rdata2            := regFile.io.rdata2

    exe_flush             := flush
 //  exe_ld_hazard         := ld_hazard
  }

  if(p(Trace)){
    printf("EXE stage\n")
    printf("rs1: REG[%d]\n",exe_rs1_addr)
    printf("rs2: REG[%d]\n",exe_rs2_addr)
    printf("\n")
  }


/*
  when(exe_ld_hazard)
  {
    if(p(Trace)){
      printf("NOP\n")
    }
    exe_inst   := Instructions.NOP
  }
*/
  //ALU
  alu.io.A := Mux(a_sel === A_RS1, exe_rs1, exe_pc)//如果A_sel=RS1，A信号为rs1，否则为fe_pc，赋给信号A
  alu.io.B := Mux(b_sel === B_RS2, exe_rs2, exe_immout)
  alu.io.alu_op := alu_op

  brCond.io.rs1 := exe_rs1
  brCond.io.rs2 := exe_rs2
  brCond.io.br_type := exe_br_type

  //pipelining
  when(reset.toBool || !stall && csr.io.expt) {
    mem_st_type   := 0.U
    mem_ld_type   := 0.U
    mem_wb_en     := false.B
    mem_csr_cmd   := 0.U
    mem_illegal   := false.B
    mem_pc_check  := false.B
  }.elsewhen(!stall && !csr.io.expt) {
    mem_pc                := exe_pc
    mem_inst              := exe_inst
    mem_immout            := exe_immout
    mem_immsel            := exe_immsel
    mem_alu               := alu.io.out
    mem_sum               := alu.io.sum
    mem_csr_in            := exe_csr_in
    mem_st_type           := exe_st_type
    mem_ld_type           := exe_ld_type
    mem_wb_sel            := exe_wb_sel
    mem_wb_en             := exe_wb_en
    mem_csr_cmd           := exe_csr_cmd
    mem_illegal           := exe_illegal
    mem_pc_check          := exe_pc_check
    mem_rs1               := exe_rs1
    mem_rs2               := exe_rs2

    mem_flush             := exe_flush
  }


  //Memory stage----------------------------------
  // D$ access
  val daddr   = Mux(stall, wb_alu, mem_sum) >> 2.U << 2.U
  val woffset = mem_sum(1) << 4.U | mem_sum(0) << 3.U
  //Dcache
  io.dcache.req.valid     := !stall && (mem_st_type.orR || mem_ld_type.orR)
  io.dcache.req.bits.addr := daddr
  io.dcache.req.bits.data := mem_rs2 << woffset
  io.dcache.req.bits.mask := MuxLookup(Mux(stall, mem_st_type, mem_st_type),
    "b0000".U, Seq(
      ST_SW ->  "b1111".U,
      ST_SH -> ("b11".U << mem_sum(1,0)),
      ST_SB -> ("b1".U  << mem_sum(1,0))))


  //pipelining
  when(reset.toBool || !stall && csr.io.expt) {
    wb_st_type   := 0.U//以下都是重置操作
    wb_ld_type   := 0.U
    wb_en        := false.B
    wb_csr_cmd   := 0.U
    wb_illegal   := false.B
    wb_pc_check  := false.B
  }.elsewhen(!stall && !csr.io.expt) {//若不重置，寄存器和cache都正常，执行下列操作
    wb_pc        := mem_pc
    wb_inst      := mem_inst
    wb_alu       := mem_alu
    wb_csr_in    := mem_csr_in
    wb_st_type   := mem_st_type
    wb_ld_type   := mem_ld_type
    wb_wb_sel    := mem_wb_sel
    wb_en        := mem_wb_en
    wb_csr_cmd   := mem_csr_cmd
    wb_illegal   := mem_illegal
    wb_pc_check  := mem_pc_check
  }



  //writeback stage---------------------------
  //load
  val loffset = wb_alu(1) << 4.U | wb_alu(0) << 3.U
  val lshift  = io.dcache.resp.bits.data >> loffset
  val load    = MuxLookup(wb_ld_type, io.dcache.resp.bits.data.zext, Seq(
    LD_LH  -> lshift(15, 0).asSInt, LD_LB  -> lshift(7, 0).asSInt,
    LD_LHU -> lshift(15, 0).zext,   LD_LBU -> lshift(7, 0).zext) )

  // CSR access
  //CSR之中各信号接收来源
  csr.io.stall    := stall
  csr.io.in       := wb_csr_in
  csr.io.cmd      := wb_csr_cmd
  csr.io.inst     := wb_inst
  csr.io.pc       := wb_pc
  csr.io.addr     := wb_alu
  csr.io.illegal  := wb_illegal
  csr.io.pc_check := wb_pc_check
  csr.io.ld_type  := wb_ld_type
  csr.io.st_type  := wb_st_type
  io.host <> csr.io.host

  val regWrite = MuxLookup(wb_wb_sel, wb_alu.zext, Seq(
    WB_MEM -> load,
    WB_PC4 -> (wb_pc + 4.U).zext,
    WB_CSR -> csr.io.out.zext) ).asUInt

  //Write_RegFile模块
  regFile.io.wen   := wb_en && !stall && !csr.io.expt
  regFile.io.waddr := wb_rd_addr
  regFile.io.wdata := regWrite
  wb_data          := regWrite


  //出现意外情况，则中止存储
  io.dcache.abort := csr.io.expt

  if (p(Trace)) {//此处为状况输出
    //输出当前PC位置，指令内容，和寄存器内容
    //printf("regfile.io.wen: %d\n",regFile.io.wen)
    when(regFile.io.wen) {
      printf("write_back: REG[%d] <- %x\n",
        Mux(regFile.io.wen, wb_rd_addr, 0.U),
        Mux(regFile.io.wen, regFile.io.wdata, 0.U))
    }
  }
}