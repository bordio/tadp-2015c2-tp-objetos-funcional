  package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Movimientos._

package object Criterios {
  type Criterio = Guerreros => Int

  case object quedarConMasEnergia extends Criterio {
    def apply(guerreros: Guerreros): Int = {
      if (guerreros._1.energia == guerreros._1.energiaMaxima) -1
      else guerreros._1.energia - guerreros._2.energia
    }
  }
  case object quedarConMenosEnergia extends Criterio {
    def apply(guerreros: Guerreros): Int = guerreros._2.energia - guerreros._1.energia
  }
}
