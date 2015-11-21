package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Movimientos._
import ar.edu.tadp.dragonball.seed._
import org.scalatest.{ShouldMatchers, FunSpec}

class MonstruoSpec extends FunSpec with ShouldMatchers {
  describe("ComerseAlOponente"){
    it("Majin Boo come a vegeta, luego a goku (quienes deben morir). Y obtiene  solamente los poderes de goku") {
      val (majinAlimentadoConVegeta: Guerrero, vegetaComido: Guerrero) = ComerseAlOponente(majinBoo,vegeta)
      val (majinAlimentadoConGoku: Guerrero, gokuComido: Guerrero) = ComerseAlOponente(majinBoo,goku)

      vegetaComido.estado should be (Muerto)
      gokuComido.estado should be (Muerto)
      majinAlimentadoConGoku.especie.movimientosEspeciales should be (goku.movimientosPropios)
      majinAlimentadoConGoku.especie.movimientosEspeciales shouldNot be (vegeta.movimientosPropios)
    }

    it("Cell come a vegeta, luego a goku (quienes deben morir). Y no absorve sus poderes, ya que no son androides") {
      val (cellAlimentadoConVegeta: Guerrero, vegetaComido: Guerrero) = ComerseAlOponente(cell,vegeta)
      val (cellAlimentadoConGokuYVegeta: Guerrero, gokuComido: Guerrero) = ComerseAlOponente(cellAlimentadoConVegeta,goku)

      vegetaComido.estado should be (Muerto)
      gokuComido.estado should be (Muerto)
      cellAlimentadoConVegeta.especie.movimientosEspeciales shouldNot be (goku.movimientosPropios)
      cellAlimentadoConGokuYVegeta.especie.movimientosEspeciales shouldNot be (vegeta.movimientosPropios)
    }

    it("Cell come a Androide16 y a Androide18 (quienes deben morir). Y absorve sus poderes") {
      val (cellAlimentadoConAndroide16: Guerrero, androide16Comido: Guerrero) = ComerseAlOponente(cell,androideDebil)
      val (cellAlimentadoConAndroide16YAndroide18: Guerrero, androide18Comido: Guerrero) = ComerseAlOponente(cellAlimentadoConAndroide16,androide18)

      androide16Comido.estado should be (Muerto)
      androide18Comido.estado should be (Muerto)
      cellAlimentadoConAndroide16.especie.movimientosEspeciales should be (androideDebil.movimientosPropios)
      cellAlimentadoConAndroide16YAndroide18.especie.movimientosEspeciales should be ((androideDebil.movimientosPropios ++ androide18.movimientosPropios).distinct)
    }

    it("Cell intenta comerse a Androide17, quien tiene mayor ki. Por lo que no logra comerlo") {
      val (cellSinComer: Guerrero, androide17SigueVivo: Guerrero) = ComerseAlOponente(cell,androide17)

      androide17SigueVivo.estado shouldNot be (Muerto)
      cellSinComer.especie.movimientosEspeciales shouldNot be (androide17.movimientosPropios)
    }
  }
}
