package ar.edu.tadp.dragonball

import org.scalatest.{ShouldMatchers, FunSpec}

class GuerreroSpec extends FunSpec with ShouldMatchers {
  val Ataques: List[Movimiento] = List(DejarseFajar, CargarKi)
  val goku: Guerrero = Guerrero("goku", List(FotoDeLaLuna, EsferaDelDragon(4)), 1000, 100, Saiyajin(Normal), Luchando, Ataques)
  val vegeta: Guerrero = Guerrero("vegeta", List(SemillaDelErmitanio), 1000, 500, Saiyajin(Normal), Luchando, Ataques)
  val trunks: Guerrero = Guerrero("trunks", List(SemillaDelErmitanio, Arma(Filosa)), 2000, 1350, Saiyajin(SuperSaiyajin(1), false), Luchando, Ataques)
  val androide18: Guerrero = Guerrero("Androide18", List(Arma(Fuego(Glock)), Municion(Glock)), 1800, 900, Androide, Luchando, Ataques)

  describe ("Constructor") {
    it ("Goku should have") {
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

  describe ("Movimientos") {
    describe ("CargarKi") {
      it ("Goku (Normal) al usar CargarKi, se le debe incrementar a 200 su energia") {
        CargarKi (goku, vegeta) ._1 .energia should be (200)
      }
      it ("Trunks (SuperSaiyajin nivel 1) al usar CargarKi, se le debe incrementar a 1500 (1350 + 1*150) su energia") {
        CargarKi (trunks, goku) ._1 .energia should be (1500)
      }
      it ("Androide18 al usar CargarKi no debe tener ningun efecto por ser Androide") {
        CargarKi (androide18, trunks) ._1 .energia should be (900)
      }
    }

    describe ("DejarseFajar") {
      it ("Goku al DejarseFajar, el contador de rounds dejandose fajar debe estar en 1") {
        DejarseFajar (goku, vegeta) ._1 .estado should be (Fajado(1))
      }
      it ("Si Goku se deja fajar 3 veces consecutivas, el contador debe contar 3") {
        DejarseFajar (DejarseFajar (DejarseFajar (goku, vegeta))) ._1 .estado should be (Fajado(3))
      }
    }

    describe ("Movimientos integrados") {
      it ("Si Goku se deja fajar 2 veces consecutivas, luego carga ki, y luego vuelve a dejarse fajar, el contador debe estar en 1") {
        val estadoActual = DejarseFajar (CargarKi (DejarseFajar (DejarseFajar (goku, vegeta))))
        estadoActual ._1 .estado should not be (Fajado(3))
        estadoActual ._1 .estado should be (Fajado(1))
      }
    }
  }
}
