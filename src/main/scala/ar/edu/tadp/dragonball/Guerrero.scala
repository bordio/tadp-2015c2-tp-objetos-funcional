package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Criterios._
import ar.edu.tadp.dragonball.Movimientos._

case class Guerrero(nombre: String,
                    items: List[Item],
                    energia: Int,
                    energiaMaxima: Int,
                    especie: Especie,
                    estado: Estado,
                    movimientos: List[Movimiento]) {

  def estas(nuevoEstado: Estado) : Guerrero = {
    copy(estado = nuevoEstado)
  }

  def actualizarEnergia(variacion: Int) = {
    val guerrero = copy(energia = (energia + variacion).max(0).min(energiaMaxima))
    if (guerrero.energia <= 0) guerrero estas Muerto
    else guerrero
  }

  def cambiarEnergiaA(nuevaEnergia: Int) = copy(energia = nuevaEnergia)

  def cambiaTuMovimientosPorLosDe(oponente: Guerrero) = {
    copy(movimientos = oponente.movimientos)
  }

  def agregarMovimientosDe(oponente: Guerrero) = {
    copy(movimientos = movimientos ++ oponente.movimientos)
  }

  def tieneItem(item: Item): Boolean = {
    item match {
      case ArmaDeFuego => items.contains(item) && items.contains(Municion(ArmaDeFuego))
      case _ => items.contains(item)
    }
  }

  def eliminarItem(item: Item) = copy(items = items.diff(List(item)))

  def quedarKOSiEnergiaMenorA300 = if (energia < 300) estas(KO) else this

  def recuperarEnergiaMaxima = copy(energia = energiaMaxima)

  def cambiarEspecieA(otraEspecie: Especie) = copy(especie = otraEspecie)

  def movimientoMasEfectivoContra(oponente: Guerrero)(unCriterio: Criterio): Movimiento = {
    movimientos.maxBy(
      mov => unCriterio(mov(this,oponente))
    )
  }

  def pelearUnRound(movimiento: Movimiento)(oponente: Guerrero): Guerreros = {
    val (atacante, defensor) = movimiento(this, oponente)
    defensor.movimientoMasEfectivoContra(atacante)(quedarConMasEnergia)(atacante, defensor)
  }

  def planDeAtaqueContra (oponente: Guerrero, cantidadDeRounds: Int) (unCriterio: Criterio) :List[Movimiento] = cantidadDeRounds match {
    case 1 => List(movimientoMasEfectivoContra(oponente)(unCriterio))
    case _ =>
      val mov = movimientoMasEfectivoContra(oponente) (unCriterio)
      val (atacanteActual, oponenteActual) = pelearUnRound(mov)(oponente)
      List(mov) ++ atacanteActual.planDeAtaqueContra(oponenteActual, cantidadDeRounds-1)(unCriterio)
  }
}

abstract class Estado

case object Luchando extends Estado
case class Fajado(rounds: Int) extends Estado
case object KO extends Estado
case object Muerto extends Estado