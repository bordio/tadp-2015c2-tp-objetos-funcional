  package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Movimientos._

package object Criterios {
  type Criterio = Guerreros => Int

  def quedarConMasEnergia(guerreros: Guerreros): Int = {
    if (guerreros._1.energia == guerreros._1.energiaMaxima) -1
    else Math.abs(guerreros._1.energia - guerreros._2.energia)
    //para el androide deberia decir que no lo deje muerto, porque si no siempre elige explotar.
  }

  def quedarConMenosEnergia(guerreros: Guerreros): Int = guerreros._2.energia - guerreros._1.energia
}
