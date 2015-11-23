package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Movimientos.Movimiento
import ar.edu.tadp.dragonball.TiposDeDigestion.TipoDigestion

//import ar.edu.tadp.dragonball.TiposDeDigestion.TipoDigestion

abstract class Especie {
  def movimientosEspeciales: List[Movimiento] = List()
}

trait Magico
trait Fusionable

case object Humano extends Especie with Fusionable
case object Androide extends Especie
case object Namekusein extends Especie with Fusionable with Magico
case object Fusion extends Especie
case class Monstruo(tipoDigestion: TipoDigestion, guerrerosComidos: List[Guerrero]) extends Especie {
  override def movimientosEspeciales = {
    tipoDigestion(guerrerosComidos)
  }
}

case class Saiyajin(estado: EstadoSaiyajin, cola: Boolean = true) extends Especie with Fusionable

abstract class EstadoSaiyajin {
  def proximoNivelZ = 1
}

case object Normal extends EstadoSaiyajin

case class SuperSaiyajin(nivel: Int) extends EstadoSaiyajin {
  override def proximoNivelZ = nivel + 1
}

case object MonoGigante extends EstadoSaiyajin