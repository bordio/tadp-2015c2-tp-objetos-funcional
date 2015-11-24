package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Criterios._
import ar.edu.tadp.dragonball.Movimientos._
import ar.edu.tadp.dragonball.TiposDeDigestion.TipoDigestion

import ar.edu.tadp.dragonball.Utils._

case class Guerrero(nombre: String,
                    items: List[Item],
                    energia: Int,
                    energiaMaxima: Int,
                    especie: Especie,
                    estado: Estado,
                    movimientosPropios: List[Movimiento]) {


  val movimientos: List[Movimiento] = {
    movimientosPropios ++ especie.movimientosEspeciales
  }

  def pegarCon(movimiento: Movimiento) = movimiento(this)(_:Guerrero)

  def cambiarEstadoA(nuevoEstado: Estado) : Guerrero = {
    copy(estado = nuevoEstado)
  }

  def actualizarEnergia(variacion: Int) = {
    val guerrero = copy(energia = (energia + variacion).max(0).min(energiaMaxima))
    if (guerrero.energia <= 0) guerrero cambiarEstadoA Muerto
    else guerrero
  }

  def cambiarEnergiaA(nuevaEnergia: Int) = copy(energia = nuevaEnergia)

  def cambiarEnergiaMaximaA(nuevaEnergia: Int) = copy(energiaMaxima = nuevaEnergia)

  def tieneItem(item: Item): Boolean = {
    item match {
      case ArmaDeFuego => items.contains(item) && items.contains(Municion(ArmaDeFuego))
      case _ => items.contains(item)
    }
  }

  def eliminarItem(item: Item) = copy(items = items.diff(List(item)))
  def eliminarItem(listaDeItems: List[Item]) = copy(items = items.diff(listaDeItems))

  def recuperarEnergiaMaxima = copy(energia = energiaMaxima)

  def cambiarEspecieA(otraEspecie: Especie) = copy(especie = otraEspecie)

  def cambiarEstadoSaiyajin(nuevoEstado: EstadoSaiyajin, tieneCola:Boolean) : Guerrero = {
    copy(especie = Saiyajin(nuevoEstado,tieneCola))
  }

  def agregarMovimientos(movimientos_nuevos: List[Movimiento]) =
    copy(movimientosPropios = movimientos_nuevos ++ movimientosPropios)

  def multiplicarEnergiaMaximaPor(multiplicador: Int) =
    copy(energiaMaxima = energiaMaxima * multiplicador)

  def tieneLas7Esferas = (1 to 7).forall(estrellas => items.contains(EsferaDelDragon(estrellas)))

  def sosAndroide = especie.equals(Androide)

  def comerseA(oponente: Guerrero, tipoDigestion: TipoDigestion, guerrerosComidos: List[Guerrero]) = {
    copy(especie = Monstruo(tipoDigestion = tipoDigestion, guerrerosComidos = guerrerosComidos :+ oponente))
  }

  def movimientoMasEfectivoContra(oponente: Guerrero)(unCriterio: Criterio): Option[Movimiento] = {
    movimientos.maxByOptionable(mov => unCriterio(this.pegarCon(mov)(oponente)))
  }

  def pelearUnRound(movimiento: Movimiento)(oponente: Guerrero): Guerreros = {
    val (atacante, defensor) = this.pegarCon(movimiento)(oponente)
    defensor.movimientoMasEfectivoContra(atacante)(quedarConMasEnergia).get(defensor)(atacante).swap
  }

  def planDeAtaqueContra(oponente: Guerrero, cantidadDeRounds: Int)(unCriterio: Criterio) :List[Option[Movimiento]] = {
    def identidad(atacante: Guerrero)(oponente: Guerrero): Guerreros = (atacante, oponente)
    cantidadDeRounds match {
      case 1 => List(movimientoMasEfectivoContra(oponente)(unCriterio))
      case _ =>
        val mov = movimientoMasEfectivoContra(oponente)(unCriterio)
        val (atacanteActual, oponenteActual) = pelearUnRound(mov.getOrElse(identidad))(oponente)
        List(mov) ++ atacanteActual.planDeAtaqueContra(oponenteActual, cantidadDeRounds - 1)(unCriterio)
    }
  }

  def pelearContra(oponente: Guerrero)(planDeAtaque: List[Option[Movimiento]]) = {
    planDeAtaque.foldLeft(SiguenPeleando(this, oponente): ResultadoPelea) {
      (resultadoAnterior, movimientoActual) => resultadoAnterior match {
        case SiguenPeleando(atacanteAnterior, oponenteAnterior) =>
          val (atacanteProximo: Guerrero, oponenteProximo: Guerrero) = atacanteAnterior.pelearUnRound(movimientoActual.get)(oponenteAnterior)
          (atacanteProximo.estado, oponenteProximo.estado) match {
            case (Muerto, Muerto) | (_, Muerto) => Ganador(atacanteProximo)
            case (Muerto, _) => Ganador(oponenteProximo)
            case _ => SiguenPeleando(atacanteProximo, oponenteProximo)
          }
        case otro => otro
      }
    }
  }
}

abstract class Estado

case object Luchando extends Estado
case class Fajado(rounds: Int) extends Estado
case object KO extends Estado
case object Muerto extends Estado

trait ResultadoPelea
case class Ganador(ganador: Guerrero) extends ResultadoPelea
case class SiguenPeleando(atacante: Guerrero, oponente: Guerrero) extends ResultadoPelea