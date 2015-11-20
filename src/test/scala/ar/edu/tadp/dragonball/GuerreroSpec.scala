package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Criterios._
import ar.edu.tadp.dragonball.Movimientos._
import ar.edu.tadp.dragonball.TiposDeDigestion._
import org.scalatest.{ShouldMatchers, FunSpec}

class GuerreroSpec extends FunSpec with ShouldMatchers {
  val Ataques: List[Movimiento] = List(DejarseFajar, CargarKi, MuchosGolpesNinja, Onda(150), Genkidama, comerseAlOponente)
  val goku: Guerrero = Guerrero("goku", List(FotoDeLaLuna, EsferaDelDragon(4)), 100, 1000, Saiyajin(Normal), Luchando, Ataques)
  val vegeta: Guerrero = Guerrero("vegeta", List(SemillaDelErmitanio), 500, 1000, Saiyajin(Normal), Luchando, Ataques)
  val trunks: Guerrero = Guerrero("trunks", List(SemillaDelErmitanio, ArmaFilosa), 1350, 2000, Saiyajin(SuperSaiyajin(1), cola = false), Luchando, Ataques)
  val androide18: Guerrero = Guerrero("Androide18", List(ArmaDeFuego, Municion(ArmaDeFuego)), 900, 1800, Androide, Luchando, Ataques ++ List(Explotar))
  val yajirobe: Guerrero = Guerrero("Yajirobe", List(SemillaDelErmitanio), 400, 400, Humano, Luchando, Ataques)
  val androideDebil: Guerrero = Guerrero("Androide16", List(ArmaDeFuego, Municion(ArmaDeFuego)), 200, 300, Androide, Luchando, Ataques ++ List(Explotar))
  val cell: Guerrero = Guerrero("Cell", List(EsferaDelDragon(3)), 1200, 3000, Monstruo(DigestionCell), Luchando, Ataques)
  val piccolo: Guerrero = Guerrero("Piccolo", List(), 1000, 1000, Namekusein, Luchando, Ataques)

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

    describe ("MuchosGolpesNinjas") {
      it ("Yajirobe le pega a androide18 pero se hace daño a si mismo") {
        MuchosGolpesNinja (yajirobe, androide18) ._1 .energia should be(390)
      }
      it ("Yajirobe le pega a androide16 que es más débil que él pero se hace daño a si mismo") {
        MuchosGolpesNinja (yajirobe, androideDebil) ._1 .energia should be(390)
      }
      it ("Yajirobe quiere pegarle a vegeta pero no logra más que hacerse daño a si mismo") {
        MuchosGolpesNinja (yajirobe, vegeta) ._1 .energia should be(380)
      }
    }

    describe ("Onda") {
      it ("Vegeta le tira un kame hame a yajirobe!") {
        Onda(150) (vegeta, yajirobe) ._1 .energia should be(350)
        Onda(150) (vegeta, yajirobe) ._2 .energia should be(100)
      }
      it ("Goku trata de tirar un kame hame de 200 pero no tiene energia suficiente") {
        Onda(200) (goku, vegeta) ._1 .energia should be(100)
      }
      it ("Trunks ingenuamente trata de atacar con Kame Hame a androide18, pero lo cura") {
        Onda(200)(trunks, androide18)._2.energia should be(1300)
        Onda(200)(trunks, androide18)._1.energia should be(1150)
      }
    }

    describe ("Genkidama") {
      it ("Goku se deja fajar 3 turnos y saca 1000") {
        val gokuDejandoseFajar = DejarseFajar (DejarseFajar (DejarseFajar (goku, trunks)))
        Genkidama (gokuDejandoseFajar) ._2 .energia should be(350)
      }
      it ("Goku asesina a vegeta con una genkidama en 3 turnos") {
        val gokuFajado = goku estas Fajado(3)
        Genkidama (gokuFajado, vegeta) ._2 .estado should be(Muerto)
      }
      it ("Goku se ceba queriendo matar a todos con su genkidama, pero al androide lo cura") {
        val gokuFajado = goku estas Fajado(3)
        Genkidama (gokuFajado, androideDebil) ._2 .energia should be(300)
      }
    }

    describe ("Explotar") {
      it ("El androide16 es muy debil, y apesar de que explota no logra matar al oponente") {
        val (androideMuerto, piccoloRePiyo) = Explotar (androideDebil, piccolo)
         androideMuerto.estado should be(Muerto)
        piccoloRePiyo.energia should be(600)
      }
      it ("El androide18 explota, y mata a yajirobe") {
        val (androideMuerto, yajirobeMuerto) = Explotar (androide18, yajirobe)
        androideMuerto.estado should be(Muerto)
        yajirobeMuerto.estado should be(Muerto)
      }
      it ("El androide18 explota, y pero no puede matar al gran Piccolo Daimaku porque es namekuseiano") {
        val (androideMuerto, piccoloExplotado) = Explotar (androide18, piccolo)
        androideMuerto.estado should be(Muerto)
        piccoloExplotado.estado should be(Luchando)
        piccoloExplotado.energia should be(1)
      }
    }

    describe ("Movimientos integrados") {
      it ("Si Goku se deja fajar 2 veces consecutivas, luego carga ki, y luego vuelve a dejarse fajar, el contador debe estar en 1") {
        val estadoActual = DejarseFajar (CargarKi (DejarseFajar (DejarseFajar (goku, vegeta))))
        estadoActual ._1 .estado should not be Fajado(3)
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
