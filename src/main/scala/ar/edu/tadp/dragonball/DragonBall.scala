package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Movimientos.Movimiento


object DragonBall {

  trait Item

  case object ArmaFilosa extends Item

  case object ArmaRoma extends Item

  case object ArmaDeFuego extends Item

  case object SemillasDelErmitanio extends Item

  case class Municion(tipo: Item) extends Item

  case object FotoDeLaLuna extends Item

  case object Esfera1Estrella extends Item

  case object Esfera2Estrella extends Item

  case object Esfera3Estrella extends Item

  case object Esfera4Estrella extends Item

  case object Esfera5Estrella extends Item

  case object Esfera6Estrella extends Item

  case object Esfera7Estrella extends Item


  trait Especie

  case class Saiyajin(nivel: Int = 0, tieneCola: Boolean = true) extends Especie

  case object Humano extends Especie

  case object Androide extends Especie

  case object Namekusein extends Especie

  class TipoDigestion {

    def movimientosAlComerA(guerrero: Guerrero, movimientosActuales: List[Movimiento]) =
      movimientosActuales ++ guerrero.movimientos
  }

  case class Monstruo(tipoDigestion: TipoDigestion) extends Especie

  case object Indefinido extends Especie

  trait Estado

  case object SuperSaiyajin extends Estado

  case object MonoGigante extends Estado

  case object Normal extends Estado

  case object KO extends Estado

  case object Muerto extends Estado

  case class PlanDeAtaque(movimientos: List[Movimiento] = List()) {
    def agregarMovimiento(movimiento: Movimiento) =
      copy(movimientos = movimientos :+ movimiento)
  }

  trait ResultadoPelea
  case class Ganador(ganador: Guerrero) extends ResultadoPelea
  case class SiguenPeleando(atacante: Guerrero, oponente: Guerrero) extends ResultadoPelea

}
