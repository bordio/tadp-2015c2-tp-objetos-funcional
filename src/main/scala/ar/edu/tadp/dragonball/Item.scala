package ar.edu.tadp.dragonball

abstract class Item

case object SemillaDelErmitanio extends Item
case object FotoDeLaLuna extends Item
case class EsferaDelDragon(estrellas: Int) extends Item
case class Arma(tipo: TipoArma) extends Item
case class Municion(tipo: TipoArmaFuego) extends Item

abstract class TipoArma

case object Roma extends TipoArma
case object Filosa extends TipoArma
case class Fuego(tipo: TipoArmaFuego) extends TipoArma

trait TipoArmaFuego

case object Colt extends TipoArmaFuego
case object Glock extends TipoArmaFuego