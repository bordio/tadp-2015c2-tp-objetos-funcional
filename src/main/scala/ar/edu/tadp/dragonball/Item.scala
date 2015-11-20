package ar.edu.tadp.dragonball

abstract class Item

case object SemillaDelErmitanio extends Item
case object FotoDeLaLuna extends Item
case class EsferaDelDragon(cantidad: Int) extends Item
case class Arma(tipo: TipoArma) extends Item
case class Municion(tipo: ArmaDeFuego) extends Item

abstract class TipoArma

case object Roma extends TipoArma
case object Filosa extends TipoArma
case class deFuego(tipo: ArmaDeFuego) extends TipoArma

trait ArmaDeFuego

case object Colt extends ArmaDeFuego
case object Glock extends ArmaDeFuego