package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Movimientos.Guerreros

package object TiposDeDigestion {
  type FormaDeDigerir = Guerreros => Guerrero

  case object DigestionCell extends FormaDeDigerir {
    def apply(guerreros: Guerreros): Guerrero = {
      val (atacante, oponente) = guerreros
      oponente.especie match {
        case Androide => atacante agregarMovimientosDe oponente
        case _ => atacante
      }
    }
  }

  case object DigestionMajinBoo extends FormaDeDigerir {
    def apply(guerreros: Guerreros): Guerrero = {
      val (atacante, oponente) = guerreros
      atacante cambiaTuMovimientosPorLosDe oponente
    }
  }
}
