package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.DragonBall._
import ar.edu.tadp.dragonball.Movimientos._

case class Guerrero(nombre: String, inventario: List[Item], energia: Int, energiaMaxima: Int, movimientos: List[Movimiento], especie: Especie, estado: Estado, tieneCola: Boolean = false, roundsDejandoseFajar: Int = 0, nivel: Int = 0) {

  val tipoDigestion = new TipoDigestion

  def realizarMovimiento(oponente: Guerrero, movimiento: Movimiento) = {
    if (movimientos.contains(movimiento)) movimiento(this, oponente) else (this, oponente)
  }

  def movimientoMasEfectivoContra(oponente: Guerrero) = {
    (criterio: Criterio) => {
      movimientos.maxBy(mov =>
        criterio.cuantificar(this.realizarMovimiento(oponente, mov)._1, this.realizarMovimiento(oponente, mov)._2))
    }
  }

  def pelearUnRound(movimiento: Movimiento) = {
    (oponente: Guerrero) => {
      val (atacante, defensor) = this.realizarMovimiento(oponente, movimiento)
      defensor.realizarMovimiento(atacante, defensor.movimientoMasEfectivoContra(this)(new CriterioEnergia))
    }
  }

  def aumentarRoundsDejandoseFajar() =
    copy(roundsDejandoseFajar = roundsDejandoseFajar + 1)

  def aumentarEnergia(cantidad: Int) = {
    val guerrero = copy(energia = energia + cantidad)

    if (guerrero.energia > guerrero.energiaMaxima) {
      copy(energia = energiaMaxima)
    } else {
      guerrero
    }
  }

  def reducirEnergia(cantidad: Int) = {
    val guerrero = copy(energia = energia - cantidad)

    if (guerrero.energia <= 0) {
      copy(energia = 0).cambiarEstadoA(Muerto)
    } else {
      guerrero
    }
  }

  def perderCola() =
    copy(tieneCola = false)

  def cambiarEnergiaA(cantidad: Int) =
    copy(energia = cantidad)

  def cambiarEstadoA(unEstado: Estado) = {
    val guerrero = copy(estado = unEstado)
    if (estado == SuperSaiyajin && unEstado != SuperSaiyajin) {
      guerrero.copy(nivel = 0)
    } else {
      guerrero
    }
  }

  def quedarKOSiEnergiaMenorA(cantidad: Int) =
    if (energia < cantidad) cambiarEstadoA(KO) else this

  def recuperarEnergiaMaxima() =
    copy(energia = energiaMaxima)

  def multiplicarEnergiaMaximaPor(multiplicador: Int) =
    copy(energiaMaxima = energiaMaxima * multiplicador)

  def aumentarEnergiaMaxima(cantidad: Int) =
    copy(energiaMaxima = energiaMaxima + cantidad)

  def tieneItem(item: Item) =

    item match {
      case ArmaDeFuego =>
        inventario.contains(item) && inventario.contains(Municion(item))
      case _ => inventario.contains(item)
    }

  def eliminarItem(item: Item) =
    copy(inventario = inventario.diff(List(item)))

  def comerseA(oponente: Guerrero) =
    copy(movimientos = tipoDigestion.movimientosAlComerA(oponente, movimientos))

  def tieneFotoDeLuna() =
    inventario.contains(FotoDeLaLuna)

  def puedeSubirDeNivel() =
    energia >= energiaMaxima / 2

  def subirNivel() =
    copy(nivel = nivel + 1)

  def eliminarEspecie() =
    copy(especie = Indefinido)

  def tieneLas7Esferas() =
    inventario.contains(Esfera1Estrella) &&
      inventario.contains(Esfera2Estrella) &&
      inventario.contains(Esfera3Estrella) &&
      inventario.contains(Esfera4Estrella) &&
      inventario.contains(Esfera5Estrella) &&
      inventario.contains(Esfera6Estrella) &&
      inventario.contains(Esfera7Estrella)
}
