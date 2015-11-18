package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Criterios._
import ar.edu.tadp.dragonball.Movimientos._

case class Guerrero(nombre: String,
                    items: List[Item],
                    energiaMaxima: Int,
                    energia: Int,
                    especie: Especie,
                    estado: Estado,
                    movimientos: List[Movimiento]) {

  def estas(nuevoEstado: Estado) : Guerrero = {
    copy(estado = nuevoEstado)
  }

  def aumentarEnergia(aumento: Int) = {
    copy(energia = (aumento + energia).max(0).min(energiaMaxima))
  }

  def movimientoMasEfectivoContra(oponente: Guerrero)(unCriterio: Criterio): Movimiento = {
    movimientos.maxBy(
      mov => unCriterio(mov(this,oponente))
    )
  }

  def pelearUnRound(movimiento: Movimiento)(oponente: Guerrero): Guerreros = {
    val (atacante, defensor) = movimiento(this, oponente)
    defensor.movimientoMasEfectivoContra(atacante)(quedarConMasEnergia)(atacante, defensor)
  }

}

abstract class Estado

case object Luchando extends Estado
case class Fajado(rounds: Int) extends Estado
case object KO extends Estado
case object Muerto extends Estado