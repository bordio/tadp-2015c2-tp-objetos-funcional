package ar.edu.tadp.dragonball

import org.scalatest.{ShouldMatchers, FunSpec}

class GuerreroSpec extends FunSpec with ShouldMatchers {
  val Ataques: List[Movimiento] = List(DejarseFajar, CargarKi)
  val goku: Guerrero = Guerrero("goku", List(FotoDeLaLuna,EsferaDelDragon), 1000, 100, Saiyajin(Normal), Luchando, Ataques)
  val vegeta: Guerrero = Guerrero("vegeta", List(SemillaDelErmitaño), 1000, 500, Saiyajin(Normal), Luchando, Ataques)
  val trunks: Guerrero = Guerrero("trunks", List(SemillaDelErmitaño, Arma(Filosa)), 2000, 1350, Saiyajin(SuperSaiyajin(1), false), Luchando, Ataques)
  val androide18: Guerrero = Guerrero("Androide18", List(Arma(Fuego(Glock)),Municion(Glock)), 1800, 900, Androide, Luchando, Ataques)

  describe("Constructor") {
    it("Goku should have") {
      goku should have(
        'nombre ("goku"),
        'energia (100),
        'energiaMaxima (1000),
        'especie (Saiyajin(Normal)),
        'estado (Luchando),
        'movimientos (Ataques)
      )
    }
  }

  describe("Movimientos") {
    describe("CargarKi") {
      it("when Goku (Normal) uses CargarKi should increase his energy to 200 points") {
        CargarKi (goku, vegeta) ._1 .energia should be (200)
      }
      it("when Trunks (SuperSaiyajin level 1) uses CargarKi should increase his energy to 1500 points") {
        CargarKi (trunks, goku) ._1 .energia should be (1500)
      }
      it("when Androide18 uses CargarKi shouldnt make any effect") {
        CargarKi (androide18, trunks) ._1 .energia should be (900)
      }
    }
  }
}
