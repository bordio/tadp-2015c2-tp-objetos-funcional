package ar.edu.tadp.dragonball

case class Guerrero(nombre: String,
                    inventario: List[Item],
                    energia: Int,
                    energiaMaxima: Int,
                    movimientos: List[Movimiento],
                    especie: Especie,
                    estado: Estado,
                    roundsDejandoseFajar: Int = 0) {

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

  def planDeAtaqueContra(oponente: Guerrero, cantidadDeRounds: Int) = {
    (criterio: Criterio) => {
      var movimientoActual = movimientoMasEfectivoContra(oponente)(criterio)
      var planDeAtaque = PlanDeAtaque(List(movimientoActual))
      var atacanteActual = this
      var oponenteActual = oponente

      for (_ <- 1 to cantidadDeRounds) {
        val (atacanteProximo: Guerrero, oponenteProximo: Guerrero) = atacanteActual.pelearUnRound(movimientoActual)(oponenteActual)
        atacanteActual = atacanteProximo
        oponenteActual = oponenteProximo
        movimientoActual = atacanteActual.movimientoMasEfectivoContra(oponenteActual)(criterio)
        planDeAtaque = planDeAtaque.agregarMovimiento(movimientoActual)
      }
      planDeAtaque
    }
  }

  def pelearContra(oponente: Guerrero) = {
    (planDeAtaque: PlanDeAtaque) => {
      planDeAtaque.movimientos.foldLeft(SiguenPeleando(this, oponente): ResultadoPelea) { (resultadoAnterior, movimientoActual) =>

        resultadoAnterior match {
          case SiguenPeleando(atacanteAnterior, oponenteAnterior) =>
            val (atacanteProximo: Guerrero, oponenteProximo: Guerrero) = atacanteAnterior.pelearUnRound(movimientoActual)(oponenteAnterior)

            (atacanteProximo.estado, oponenteProximo.estado) match {
              case (Muerto, Muerto) | (_, Muerto) => Ganador(atacanteProximo)
              case (Muerto, _) => Ganador(oponenteProximo)
              case (_) => SiguenPeleando(atacanteProximo, oponenteProximo)
            }
          case otro => otro
        }
      }
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

  def cambiarEnergiaA(cantidad: Int) =
    copy(energia = cantidad)

  def cambiarEstadoA(unEstado: Estado) = {
    val guerrero = copy(estado = unEstado)
    if (estado == SuperSaiyajin && unEstado != SuperSaiyajin) {
      guerrero.especie match {
        case Saiyajin(_, tieneCola) =>
          guerrero.cambiarEspecieA(Saiyajin(0, tieneCola))
      }
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

  def comerseA(oponente: Guerrero, tipoDigestion: TipoDigestion) =
    copy(movimientos = tipoDigestion.movimientosAlComerA(oponente, movimientos))

  def tieneFotoDeLuna() =
    inventario.contains(FotoDeLaLuna)

  def puedeSubirDeNivel() =
    energia >= energiaMaxima / 2

  def cambiarEspecieA(unaEspecie: Especie) =
    copy(especie = unaEspecie)

  def tieneLas7Esferas() =
    (1 to 7).forall(estrellas =>
      inventario.contains(Esfera(estrellas)))
}
