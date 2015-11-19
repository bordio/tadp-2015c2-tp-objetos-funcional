package ar.edu.tadp.dragonball

abstract class Especie

trait Magico
trait Fusionable

case object Humano extends Especie with Fusionable
case object Androide extends Especie
case object Namekusein extends Especie with Fusionable with Magico
case object Monstruo extends Especie with Magico
case class Saiyajin(estado: EstadoSaiyajin, cola: Boolean = true) extends Especie with Fusionable

abstract class EstadoSaiyajin {
  def proximoNivelZ = 1
}

case object Normal extends EstadoSaiyajin

case class SuperSaiyajin(nivel: Int) extends EstadoSaiyajin {
  override def proximoNivelZ = nivel + 1
}

case object MonoGigante extends EstadoSaiyajin