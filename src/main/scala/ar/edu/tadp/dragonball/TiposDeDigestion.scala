package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Movimientos.Movimiento

package object TiposDeDigestion {
  type TipoDigestion = List[Guerrero] => List[Movimiento]

  val digestionCell: TipoDigestion = (guerreros: List[Guerrero]) => {
    guerreros.filter(_. sosAndroide).flatMap(_. movimientosPropios).distinct
  }

  val digestionMajinBoo: TipoDigestion = (guerreros: List[Guerrero]) => {
    val guerrero = guerreros.lastOption
    if (guerrero.isEmpty) List() else guerrero.get.movimientosPropios
  }
}
