package ar.edu.tadp.dragonball

abstract class Item

case object SemillaDelErmitanio extends Item
case object FotoDeLaLuna extends Item
case class EsferaDelDragon(estrellas: Int) extends Item
case object ArmaFilosa extends Item
case object ArmaRoma extends Item

trait ArmaConMunicion
case object ArmaDeFuego extends Item with ArmaConMunicion
case class Municion(tipo: ArmaConMunicion) extends Item
