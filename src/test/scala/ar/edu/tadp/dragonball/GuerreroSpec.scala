package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Criterios._
import ar.edu.tadp.dragonball.Movimientos._
import ar.edu.tadp.dragonball.TiposDeDigestion._
import org.scalatest.{ShouldMatchers, FunSpec}

class GuerreroSpec extends FunSpec with ShouldMatchers {

  val Ataques: List[Movimiento] = List(DejarseFajar, CargarKi, MuchosGolpesNinja, Onda(150), Genkidama)
  val AtaquesDeYajirobe: List[Movimiento] = List(MuchosGolpesNinja, UsarItem(ArmaFilosa), UsarItem(SemillaDelErmitanio))
  val goku: Guerrero = Guerrero("goku", List(FotoDeLaLuna, EsferaDelDragon(4)), 100, 1000, Saiyajin(Normal,false), Luchando, Ataques)
  val vegeta: Guerrero = Guerrero("vegeta", List(FotoDeLaLuna,SemillaDelErmitanio), 500, 1000, Saiyajin(Normal), Luchando, Ataques ++ List(UsarItem(FotoDeLaLuna)))
  val trunks: Guerrero = Guerrero("trunks", List(SemillaDelErmitanio, ArmaFilosa), 1350, 2000, Saiyajin(SuperSaiyajin(1), cola = false), Luchando, Ataques)
  val androide18: Guerrero = Guerrero("Androide18", List(ArmaDeFuego, Municion(ArmaDeFuego)), 900, 1800, Androide, Luchando, Ataques ++ List(Explotar,UsarItem(FotoDeLaLuna)))
  val androide17: Guerrero = Guerrero("Androide17", List(ArmaDeFuego, Municion(ArmaDeFuego)), 1800, 5000, Androide, Luchando, Ataques ++ List(Explotar,UsarItem(FotoDeLaLuna)))
  val yajirobe: Guerrero = Guerrero("Yajirobe", List(SemillaDelErmitanio), 400, 400, Humano, Luchando, AtaquesDeYajirobe)
  val androideDebil: Guerrero = Guerrero("Androide16", List(ArmaDeFuego, Municion(ArmaDeFuego)), 200, 300, Androide, Luchando, Ataques ++ List(Explotar, Onda(80)))
  val cell: Guerrero = Guerrero("Cell", List(EsferaDelDragon(3)), 1200, 3000, Monstruo(digestionCell,List()), Luchando, Ataques ++ List(ComerseAlOponente))
  val majinBoo: Guerrero = Guerrero("MajinBoo", List(), 2000, 3000, Monstruo(digestionMajinBoo, List()), Luchando, Ataques ++ List(ComerseAlOponente))
  val piccolo: Guerrero = Guerrero("Piccolo", List(), 1000, 1000, Namekusein, Luchando, Ataques)

  describe ("Constructor") {
    it ("Goku should have") {
      goku should have(
        'nombre ("goku"),
        'energia (100),
        'energiaMaxima (1000),
        'especie (Saiyajin(Normal,false)),
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

    describe ("ConvertirseEnMono"){
      it("MajinBu intenta convertirse en mono y no puede, al no ser un saiyajin"){
        ConvertirseEnMono(majinBoo,goku) ._1 .especie shouldNot be (Saiyajin(MonoGigante,true))
      }

      it("Goku intenta convertirse en mono y no puede, no tiene cola"){
        ConvertirseEnMono(goku,majinBoo) ._1 .especie shouldNot be (Saiyajin(MonoGigante,true))
      }

      it("Vegeta intenta convertirse en mono y puede, ya que tiene cola y foto de la luna"){
        ConvertirseEnMono(vegeta,majinBoo) ._1 .especie should be (Saiyajin(MonoGigante,true))
      }
    }

    describe("ConvertirseEnSuperSaiyajin"){

      it("MajinBu intenta convertirse en SS y no puede, al no ser un saiyajin"){
        ConvertirseEnSuperSaiyajin(majinBoo,goku) ._1 .especie shouldNot be (Saiyajin(SuperSaiyajin(1),true))
      }

      it("Goku intenta convertirse en superSaiyajin. No puede al no alcanzarle su energia"){
        ConvertirseEnSuperSaiyajin(goku,vegeta) ._1 .especie shouldNot be (Saiyajin(SuperSaiyajin(1),false))
      }

      it("Vegeta intenta convertirse en superSaiyajin. Al hacerlo si ki maximo se multiplica por 5. Su ki sigue igual"){
        val (vegetaSS1: Guerrero, vegetaSigueIgual: Guerrero) = ConvertirseEnSuperSaiyajin(vegeta,goku)
        vegetaSS1.especie should be (Saiyajin(SuperSaiyajin(1),true))
        vegetaSS1.energia should be (vegeta.energia)
        vegetaSS1.energiaMaxima should be (vegeta.energiaMaxima * 5)
      }
    }

    describe("Fusionarse"){
      it("Goku se fusiona con Vegeta"){
        val (fusionado:Guerrero,vegetaSigueIgual:Guerrero) = Fusionarse(goku)(vegeta,androide17)
        fusionado.especie should be (Fusion)
        fusionado.energia should be (vegeta.energia + goku.energia)
        fusionado.energiaMaxima should be (vegeta.energiaMaxima + goku.energiaMaxima)
        fusionado.movimientos should be (vegeta.movimientos ++ goku.movimientos)
      }

      it("Goku intenta fusionarse con Cell, no lo logra ya que cell no es fusionable"){
        val (fusionado:Guerrero,cellSigueIgual:Guerrero) = Fusionarse(goku)(cell,androide17)
        (fusionado,cellSigueIgual) should be (goku,cell)
        fusionado.especie shouldNot be (Fusion)
      }
    }

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
      it ("aa") {
        androide18.movimientoMasEfectivoContra(yajirobe)(quedarConMasEnergia) should be(Onda(150))
      }
    }

    describe ("pelearUnRound") {
      it ("Goku se deja fajar en round, pero vegeta no aprovecha la oportunidad") {
        val (gokuDespues, vegetaDespues) = goku.pelearUnRound(DejarseFajar)(vegeta)
        gokuDespues.estado should be(Fajado(1))
      }
      it ("El androide18 saca a pasear a Yajirobe") {
        val (yajirobeDespues, androideDespues) = yajirobe.pelearUnRound(MuchosGolpesNinja)(androide18)
        yajirobeDespues.energia should be(90) //se hizo 10 a si mismo, y el androide lo ataco con Onda(150)
      }
    }

    describe ("planDeAtaqueContra") {
      it ("Goku para quedar con mas energia durante dos turnos, siempre elige CargarKi") {
        goku.planDeAtaqueContra(vegeta, 2)(quedarConMasEnergia) should be(List(CargarKi, Onda(150)))
      }
      it ("Cell es demasiado groso para piccolo, y de guapo no mas, se deja Fajar 3 turnos seguidos.") {
        cell.planDeAtaqueContra(piccolo, 3)(quedarConMenosEnergia) should be(List(DejarseFajar, DejarseFajar, DejarseFajar))
      }
      it ("Yajirobe ataca a Goku") {
        yajirobe.planDeAtaqueContra(cell, 2)(quedarConMasEnergia) should be(List(UsarItem(ArmaFilosa), UsarItem(SemillaDelErmitanio)))
      }
    }
  }
}
