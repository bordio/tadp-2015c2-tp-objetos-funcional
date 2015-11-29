package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Criterios._
import ar.edu.tadp.dragonball.Movimientos._
import ar.edu.tadp.dragonball.TiposDeDigestion.TipoDigestion

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
    movimientos.map(mov => (mov, unCriterio(this.pegarCon(mov)(oponente))))
               .sortBy(movimientoConPuntaje => - movimientoConPuntaje._2)
               .filter(movimientoConPuntaje => movimientoConPuntaje._2 > 0)
               .map(movimientoConPuntaje => movimientoConPuntaje._1)
               .headOption
  }

  def pelearUnRound(movimiento: Movimiento)(oponente: Guerrero): Guerreros = {
    def identidad(atacante: Guerrero)(oponente: Guerrero) = (atacante, oponente)
    val (atacante, defensor) = this.pegarCon(movimiento)(oponente)
    defensor.movimientoMasEfectivoContra(atacante)(quedarConMasEnergia).getOrElse(identidad (_))(defensor)(atacante).swap
  }

  def planDeAtaqueContra(oponente: Guerrero, cantidadDeRounds: Int)(unCriterio: Criterio) :List[Movimiento] = {
    cantidadDeRounds match {
      case 1 => List(movimientoMasEfectivoContra(oponente)(unCriterio).getOrElse(throw new Exception("No es posible armar un plan")))
      case _ =>
        val mov = movimientoMasEfectivoContra(oponente)(unCriterio).getOrElse(throw new Exception("No es posible armar un plan"))
        val (atacanteActual, oponenteActual) = pelearUnRound(mov)(oponente)
        List(mov) ++ atacanteActual.planDeAtaqueContra(oponenteActual, cantidadDeRounds - 1)(unCriterio)
    }
  }

  def pelearContra(oponente: Guerrero)(planDeAtaque: List[Movimiento]): ResultadoPelea = {
    planDeAtaque.foldLeft(SiguenPeleando(this, oponente): ResultadoPelea) {
      (resultadoAnterior, movimiento) => resultadoAnterior.map(movimiento) }
  }
}

abstract class Estado

case object Luchando extends Estado
case class Fajado(rounds: Int) extends Estado
case object KO extends Estado
case object Muerto extends Estado

trait ResultadoPelea {
  def map(f: (Guerrero) => (Guerrero) => (Guerrero, Guerrero)): ResultadoPelea
  def flatMap(f:(Guerrero) => (Guerrero) => ResultadoPelea): ResultadoPelea
  def fold(valorInicial: Guerrero)(f: (Guerrero) => (Guerrero) => Guerrero): Guerrero
  def filter(f: (Guerrero) => Boolean): ResultadoPelea
}
case class Ganador(ganador: Guerrero) extends ResultadoPelea {
  override def map(f: (Guerrero) => (Guerrero) => (Guerrero, Guerrero)): ResultadoPelea = this
  override def flatMap(f: (Guerrero) => (Guerrero) => ResultadoPelea): ResultadoPelea = this
  override def fold(t: Guerrero)(f: (Guerrero) => (Guerrero) => (Guerrero)): Guerrero = ganador
  override def filter(f: (Guerrero) => Boolean): ResultadoPelea = this
}
case class SiguenPeleando(atacante: Guerrero, oponente: Guerrero) extends ResultadoPelea {
  override def map(f: (Guerrero) => (Guerrero) => (Guerrero, Guerrero)): ResultadoPelea = {
    val (guerrero1, guerrero2) = atacante.pelearUnRound(f)(oponente)
    (guerrero1.estado, guerrero2.estado) match {
      case (_ , Muerto) => Ganador(guerrero1)
      case (Muerto, _) => Ganador(guerrero2)
      case (_, _) => SiguenPeleando(guerrero1, guerrero2)
    }
  }
  override def flatMap(f: (Guerrero) => (Guerrero) => ResultadoPelea): ResultadoPelea = f(atacante)(oponente)
  override def filter(f: (Guerrero) => Boolean): ResultadoPelea = this
  override def fold(t: Guerrero)(f: (Guerrero) => (Guerrero) => (Guerrero)): Guerrero = f(atacante)(oponente)
}