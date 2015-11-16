package ar.edu.tadp.dragonball

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
}

abstract class Estado

case object Luchando extends Estado
case class Fajado(rounds: Int) extends Estado
case object KO extends Estado
case object Muerto extends Estado