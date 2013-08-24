package scala.tools.virtualclasses

import scala.tools.nsc._
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform._
import scala.tools.nsc.ast.TreeDSL

/**
 *
 */

abstract class VCFinalBindingsTransform(val global: Global) extends PluginComponent with Transform
with TypingTransformers with InfoTransform with Commons {
  import global._

  override val phaseName = "vc_finalbindings"

  override def transformInfo(sym: Symbol, tpe: Type) =
    infoTransformer(tpe)

  private object infoTransformer extends TypeMap {
    def apply(tpe: Type) = {
      val tpe0 = mapOver(tpe)
      tpe0 match {
        case ClassInfoType(parents, decls0, clazz) if (containsInitialBindings(clazz)) =>

          val decls = newScope
          def enter(sym: Symbol) =
          /*atPhase(ownPhase.next) {*/ decls.enter(sym) /*}*/
          for (m <- decls0) {
            enter(m)
            if (isInitialBinding(m))
              enter(mkFinalBindingSym(m))
          }

          ClassInfoType(parents map this, decls, clazz)

        case tr @ TypeRef(_,_,_) =>
          //println(tr.pre.toLongString)
          //println(tr.toLongString)
          //println(tr.sym)
          tr
          
        case x => x
      }
    }
  }

  def mkFinalBindingSym(initBinding: Symbol): Symbol = {
    val fbname = finalBindingName(initBinding)
    val fbsym = initBinding.owner.enclClass.newClass(fbname, initBinding.pos).setFlag(SYNTHETIC)
    //val parents = List(definitions.ObjectClass.tpe, initBinding.tpe)
    val parents = List(initBinding.tpe)
    val scope = newScope
    fbsym setInfo ClassInfoType(parents, scope, fbsym)

    //for every abstract type, create alias and provide factory definition
    for (abstpe <- initBinding.info.decls.filter(isVCAbstractType)) {
      //alias type
      //val absTpeBinding = abstpe.cloneSymbol(fbsym)
      val workerTrait = initBinding.info.decl(workerTraitName(abstpe)).cloneSymbol(fbsym)
      scope enter workerTrait
      
      //absTpeBinding setInfo workerTrait.tpe.substThis(initBinding, ThisType(fbsym))
      //val absTpeBindingInfo = absTpeBinding.info.asInstanceOf[TypeBounds]
      
      println(workerTrait.tpe)
      val absTpeBinding = fbsym.newAliasType(fbsym.pos, abstpe.name.toTypeName) //typeRef(fbsym.thisType, workerTrait, List())
      absTpeBinding setInfo workerTrait.tpe.substThis(initBinding, ThisType(fbsym))
      
      absTpeBinding.resetFlag(DEFERRED)
      scope enter absTpeBinding

      //factory
      val factory = fbsym.newMethod(fbsym.pos, factoryName(abstpe))//initBinding.info.decl(factoryName(abstpe)).cloneSymbol(fbsym).asInstanceOf[MethodSymbol]
      //factory.resetFlag(DEFERRED)
      //factory setInfo factory.tpe.substSym(List(abstpe), List(workerTrait))
      //factory setInfo factory.tpe.substThis(initBinding, ThisType(fbsym))
      val factoryInfo = new MethodType(List(), absTpeBinding.cloneSymbol.setInfo(initBinding.info.decl(workerTraitName(abstpe)).cloneSymbol.tpe).toType)//fbsym(factory.pos, factory.name)
      //factoryInfo setInfo absTpeBinding.tpe
      factory setInfo factoryInfo
      //factory setInfo typeRef(fbsym.thisType, absTpeBinding, List())
      //factory.setTypeSignature(absTpeBinding.tpe)
      scope enter factory
    }

    fbsym
  }

  def newTransformer(unit: CompilationUnit) = new Transformer(unit)

  class Transformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    protected def finalBindingSym(vc: Symbol): Symbol = {
      atPhase(ownPhase.next) {
        vc.owner.enclClass.info.member(finalBindingName(vc))
      }
    }

    protected def mkConcreteClassSym(factory: Symbol, finalSym: Symbol, initBinding: Symbol) = {
      val workerTraitSym = factory.enclClass.info.member(workerTraitName(factory))
      val cclazz = finalSym.newClass(finalSym.pos, concreteClassName(factory))
        .setFlag(SYNTHETIC)
        .setAnnotations(initBinding.annotations)
      val parentClass = (workerTraitSym.info.baseClasses.dropWhile(_.isTrait)).head.tpe
      val mixins = (workerTraitSym.info.baseClasses.filter(_.isTrait)).distinct.reverse.map(_.tpe)
      val parents = (parentClass :: mixins).map(_.substThis(initBinding, ThisType(factory.enclClass)).substSym(workerTraitSym.typeParams, factory.typeParams)) //TODO is this sufficient?

      cclazz setInfo ClassInfoType(parents, newScope, cclazz)
      cclazz
    }

    protected def mkFactoryDefDef(factory: Symbol, finalBinding: Symbol, fixSymbols: List[Tree]): Tree = {
      val cclazzSym = fixSymbols.filter(a => a.symbol.name == concreteClassName(factory))(0).symbol

      val args = factory.paramss map (_.map(Ident)) //TODO clone or not?
      val body = localTyper.typed { New(Select(This(factory.enclClass), cclazzSym), args) }

      val workerTraitSym = factory.enclClass.info.member(workerTraitName(factory))
      
      //localTyper.typedPos(factory.enclClass.pos) {
        
       DefDef(factory, Modifiers(factory.flags), body).setType(NoType)
      /*DefDef(Modifiers(0),
             factory.name.toTermName,
             factory.typeParams map TypeDef,
             List(),
             TypeTree(workerTraitSym.tpe.finalResultType) setPos factory.pos.focus,
             body) setSymbol factory*/
      //}
    }

    protected def mkFinalBinding(initBinding: Symbol): Tree = {
      val finalBinding = finalBindingSym(initBinding)
      val tpeBindings = finalBinding.info.members.toList.filter(isVCAbstractType) //TODO should we distinguish between abstract type and its concrete binding? If so, use another predicate
      val factories = finalBinding.info.members.toList.filter(_.name.startsWith(FACTORYPREFIX))

      def mkAbsTpeBinding(tpeSym: Symbol): Tree = {
        val workerTrait = finalBinding.info.member(workerTraitName(tpeSym))
        localTyper.typedPos(initBinding.pos) {
          TypeDef(tpeSym, TypeTree(workerTrait.tpe.substThis(initBinding, finalBinding)))
        }
      }

      def mkFixClassBindings(factory: Symbol): Tree = {
        val cclazzSym = mkConcreteClassSym(factory, finalBinding, initBinding)
        val cclazzDef = ClassDef(cclazzSym, NoMods, List(List()), List(List()), List(), factory.enclClass.pos.focus)

        localTyper.typed {
          cclazzDef
        }
      }

      val fixClasses = (factories map mkFixClassBindings)

      val body: List[Tree] = (tpeBindings map mkAbsTpeBinding) ::: fixClasses ::: (factories map (mkFactoryDefDef(_, finalBinding, fixClasses)))

      val termSym = finalBinding.newValue(finalBinding.pos, finalBinding.name).setInfo(NoType)
      val classDef = ClassDef(finalBinding, Template(List(Ident(initBinding)), emptyValDef, Modifiers(), List(), List(List()), body, initBinding.pos).setSymbol(termSym))//ClassDef(finalBinding, Modifiers(0), List(List()), List(List()), body, initBinding.pos) //TODO which modifiers?

      //global.analyzer.UnTyper.traverse(classDef)
      
      localTyper.typedPos(finalBinding.pos) {
          classDef
      }
    }

    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      stats flatMap transformStat
    }

    protected def transformStat(tree: Tree): List[Tree] = {
      val sym = tree.symbol
      tree match {
        case cd @ ClassDef(mods, name, tparams, templ) if (isInitialBinding(sym)) =>
          val fbtree = mkFinalBinding(sym)
          val newCd = ClassDef(mods, name, tparams, transform(templ).asInstanceOf[Template])
          newCd.copyAttrs(cd)
          List(cd, fbtree)
        case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          println("tpt: " + dd + "; " + dd.tpt.tpe + "; " + dd.tpt.tpe.getClass().getName() + "; pre: " + dd.tpt.tpe.prefix + "; sym: " + dd.tpt.tpe.asInstanceOf[TypeRef].sym)
          List(dd)
          
          
        case _ => List(transform(tree))
      }
    }

    override def transform(tree0: Tree): Tree = {
      val tree = super.transform(tree0)

      postTransform(tree setType atPhase(ownPhase) {
        infoTransformer(tree.tpe)
      })
    }

    protected def postTransform(tree: Tree): Tree = {
      tree match {
        //replace new calls to family class with the final implementation of the family class
        case app @ Apply(sel @ Select(New(tpt), nme.CONSTRUCTOR), args) if (isInitialBinding(app.symbol.owner) && app.symbol.isConstructor) =>
          localTyper.typed {
            atPos(tree.pos) {
              Apply(Select(New(finalBindingSym(app.symbol.owner)), nme.CONSTRUCTOR), args)
            }
          }
        case t => t
      }
    }
  }

}
