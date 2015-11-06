package ar.edu.tadp.dragonball

trait Criterio {
  def cuantificar(atacante: Guerrero, oponente: Guerrero): Int
}

class CriterioEnergia extends Criterio {
  override def cuantificar(atacante: Guerrero, oponente: Guerrero): Int = {
    atacante.energia
  }
}
