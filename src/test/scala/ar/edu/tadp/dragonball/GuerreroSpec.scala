package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Criterios._
import ar.edu.tadp.dragonball.Movimientos._
import org.scalatest.{ShouldMatchers, FunSpec}

class GuerreroSpec extends FunSpec with ShouldMatchers {
  val Ataques: List[Movimiento] = List(DejarseFajar, CargarKi, MuchosGolpesNinja, Onda(150), Genkidama)
  val goku: Guerrero = Guerrero("goku", List(FotoDeLaLuna, EsferaDelDragon(4)), 1000, 100, Saiyajin(Normal), Luchando, Ataques)
  val vegeta: Guerrero = Guerrero("vegeta", List(SemillaDelErmitanio), 1000, 500, Saiyajin(Normal), Luchando, Ataques)
  val trunks: Guerrero = Guerrero("trunks", List(SemillaDelErmitanio, Arma(Filosa)), 2000, 1350, Saiyajin(SuperSaiyajin(1), false), Luchando, Ataques)
  val androide18: Guerrero = Guerrero("Androide18", List(Arma(deFuego(Glock)), Municion(Glock)), 1800, 900, Androide, Luchando, Ataques ++ List(Explotar))
  val yajirobe: Guerrero = Guerrero("Yajirobe", List(SemillaDelErmitanio), 400, 400, Humano, Luchando, Ataques)

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

    describe ("movimientoMasEfectivoContra") {
      it ("Goku elige CargarKi porque lo deja con mas energia") {
        goku.movimientoMasEfectivoContra(vegeta)(quedarConMasEnergia) should be(CargarKi)
      }
      it ("Vegeta elige Onda(150) para quedar con menos energia") {
        vegeta.movimientoMasEfectivoContra(goku)(quedarConMenosEnergia) should be(Onda(150))
      }
      it ("Si Goku pelea contra un Androide y quiere quedar con menos energia, entonces debe elegir MuchosGolpesNinjas") {
        goku.movimientoMasEfectivoContra(androide18)(quedarConMenosEnergia) should be(MuchosGolpesNinja)
      }
    }

    describe ("planDeAtaqueContra") {
      it ("Goku para quedar con mas energia durante dos turnos, siempre elige CargarKi") {
        goku.planDeAtaqueContra(vegeta, 2) (quedarConMasEnergia) should be(List(CargarKi, CargarKi))
      }
      it ("A Androide18 para quedar con mas energia frente a trunks le conviene siempre que la traten de cagar a palos, total trunks siempre ataca con energia") {
        androide18.planDeAtaqueContra(trunks, 3)(quedarConMasEnergia) should be(List(DejarseFajar,DejarseFajar,DejarseFajar))
      }
      it ("Yajirobe ataca a Goku") {
        yajirobe.planDeAtaqueContra(goku,2)(quedarConMasEnergia) should be(List(Onda(150), CargarKi))
      }
    }
  }
}
