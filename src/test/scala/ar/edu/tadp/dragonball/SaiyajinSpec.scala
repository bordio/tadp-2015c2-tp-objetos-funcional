package ar.edu.tadp.dragonball

import ar.edu.tadp.dragonball.Movimientos._
import ar.edu.tadp.dragonball.seed._
import org.scalatest._

class SaiyajinSpec extends FunSpec with ShouldMatchers {
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

    it("Vegeta intenta convertirse en superSaiyajin. Al hacerlo, su ki maximo se multiplica por 5. Su ki sigue igual"){
      val (vegetaSS1: Guerrero, vegetaSigueIgual: Guerrero) = ConvertirseEnSuperSaiyajin(vegeta,goku)
      vegetaSS1.especie should be (Saiyajin(SuperSaiyajin(1),true))
      vegetaSS1.energia should be (vegeta.energia)
      vegetaSS1.energiaMaxima should be (vegeta.energiaMaxima * 5)
    }

    it("VegetaSS1 se convierte a SS2. Al hacerlo, su ki maximo se multiplica por 5. Su ki sigue igual"){
      val (vegetaSS1: Guerrero, vegetaSigueIgual: Guerrero) = ConvertirseEnSuperSaiyajin(vegeta,goku)
      val (vegetaSS2: Guerrero, vegetaSS1SigueIgual: Guerrero) = ConvertirseEnSuperSaiyajin(vegetaSS1.actualizarEnergia(3000),goku)
      vegetaSS2.especie should be (Saiyajin(SuperSaiyajin(2),true))
      vegetaSS2.energia should be (vegetaSS1.energia+3000)
      vegetaSS2.energiaMaxima should be (vegetaSS1.energiaMaxima * 5)
    }
  }
}
