package ar.edu.tadp

package object dragonball {
  type Guerreros = (Guerrero, Guerrero)
  type Criterio = Guerreros => Int

  abstract class Movimiento {
    def movimiento(guerreros: Guerreros): Guerreros
    def apply(guerreros: Guerreros) = {
      val (atacante, oponente) = guerreros
      (atacante.estado, this) match {
        case (Muerto, _) => guerreros
        case (KO, _) => guerreros
        case (Luchando, _) => movimiento(guerreros)
        case (Fajado(_), DejarseFajar) => movimiento(guerreros)
        case (Fajado(_), _) => movimiento(atacante estas Luchando, oponente)
      }
    }
  }

  case object DejarseFajar extends Movimiento {
    override def movimiento(guerreros: Guerreros) = {
      val (atacante, oponente) = guerreros
      atacante.estado match {
        case Luchando => (atacante estas Fajado(1), oponente)
        case Fajado(rounds) => (atacante estas Fajado(rounds + 1), oponente)
        case _ => guerreros
      }
    }
  }

  case object CargarKi extends Movimiento {
    override def movimiento(guerreros: Guerreros) = {
      val (atacante, oponente) = guerreros
      atacante.especie match {
        case Saiyajin(SuperSaiyajin(nivel), _) => (atacante aumentarEnergia (150 * nivel), oponente)
        case Androide => guerreros
        case _ => (atacante aumentarEnergia 100, oponente)
      }
    }
  }
}
