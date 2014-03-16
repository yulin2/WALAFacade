package edu.illinois.wala

import com.ibm.wala.ipa.slicer.Statement
import com.ibm.wala.ipa.cfg.BasicBlockInContext
import com.ibm.wala.ssa.analysis.IExplodedBasicBlock

trait Wrapper {
  implicit def statementToS(s: Statement) = new {
    def n = s.getNode
  }
  implicit def basicBlockInContextToS(s: BasicBlockInContext[IExplodedBasicBlock]) = s match {
    case s if s.getNode() == null || s.getLastInstruction() == null => throw new NullPointerException
    case s => S(s.getNode(), s.getLastInstruction())
  }
}