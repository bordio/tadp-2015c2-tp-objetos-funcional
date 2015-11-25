  package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Movimientos._

package object Criterios {
  type Criterio = Guerreros => Int

  def quedarConMasEnergia(guerreros: Guerreros): Int = {
    if (guerreros._1.energia == guerreros._1.energiaMaxima) -1
    else guerreros._1.energia - guerreros._2.energia
  }

  def quedarConMenosEnergia(guerreros: Guerreros): Int = guerreros._2.energia - guerreros._1.energia
}
