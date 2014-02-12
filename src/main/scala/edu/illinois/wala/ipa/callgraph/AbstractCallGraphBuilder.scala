package edu.illinois.wala.ipa.callgraph

import com.ibm.wala.ipa.callgraph.impl.Util
import com.ibm.wala.ipa.callgraph.propagation.cfa.DefaultSSAInterpreter
import com.ibm.wala.ipa.callgraph.AnalysisCache
import com.ibm.wala.ipa.cha.ClassHierarchy
import com.ibm.wala.ipa.callgraph.ContextSelector
import com.ibm.wala.ipa.callgraph.propagation.cfa.ZeroXInstanceKeys
import com.ibm.wala.analysis.pointers.HeapGraph
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.impl.ContextInsensitiveSelector
import com.ibm.wala.ipa.callgraph.impl.Util
import com.ibm.wala.ipa.callgraph.propagation.cfa.ZeroXInstanceKeys.ALLOCATIONS
import com.ibm.wala.ipa.callgraph.MethodTargetSelector
import com.ibm.wala.classLoader.IMethod
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.classLoader.CallSiteReference
import com.ibm.wala.classLoader.IClass
import collection.JavaConversions._

trait AbstractCallGraphBuilder {
  def _options: AnalysisOptions
  def _cache: AnalysisCache
  def _cha: ClassHierarchy

  // public
  def heap: HeapGraph
  def cg: CallGraph

  val exceptClasses = Set(
    "Landroid/view/ViewConfiguration")
  val methods = Set(
      "onClick",
      "onMenuItemClick",
      "onKey",
      "run")

  // just helpers
  lazy val defaultInterpreter = new DefaultSSAInterpreter(_options, _cache)
  //  lazy val reflectionInterpreter = new DelegatingSSAContextInterpreter(
  //    ReflectionContextInterpreter.createReflectionContextInterpreter(_cha, _options, _cache), defaultInterpreter)
  Util.addDefaultSelectors(_options, _cha)
  Util.addDefaultBypassLogic(_options, _options.getAnalysisScope(), classOf[Util].getClassLoader(), _cha)
  val oldSelector = _options.getMethodTargetSelector()
  _options.setSelector(new MethodTargetSelector() {
    def getCalleeTarget(n: CGNode, site: CallSiteReference, iclass: IClass): IMethod = {
      if (n.getMethod().toString().contains("<init>")
        && site.toString().contains("Ljava/lang/Object, <init>")) {
        val icls = Option(iclass) getOrElse { _cha.lookupClass(n.getMethod().getDeclaringClass().getReference() /*site.getDeclaredTarget().getDeclaringClass()*/ ) }
        if (exceptClasses.contains(icls.getName().toString()))
          oldSelector.getCalleeTarget(n, site, iclass)
        else {
          val theEventMethod = icls.getAllMethods().find(m => {
            val mName = m.getName().toString()
            methods.contains(mName)
          })
          //        if (theEventMethod.isDefined) {
          //          println("N is " + n + "\n for site" + site)
          //          println(n.getIR())
          //          println("found the event:" + theEventMethod.get + icls.getName())
          //        }
          theEventMethod getOrElse {
            oldSelector.getCalleeTarget(n, site, iclass)
          }
        }
      } else {
        oldSelector.getCalleeTarget(n, site, iclass)
      }
    }
  })

  // Hooks
  def policy = { import ZeroXInstanceKeys._; ALLOCATIONS }
  protected def cs: ContextSelector = new ContextInsensitiveSelector() // new DefaultContextSelector(_options, _cha)
  protected def contextInterpreter = defaultInterpreter // new DelegatingSSAContextInterpreter(defaultInterpreter, reflectionInterpreter)
  protected def instanceKeys = new ZeroXInstanceKeys(_options, _cha, theContextInterpreter, policy)

  // the rest...
  val theContextInterpreter = contextInterpreter
}