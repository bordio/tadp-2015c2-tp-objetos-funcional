package ar.edu.tadp.dragonball

package object Movimientos {
  type Guerreros = (Guerrero, Guerrero)
  type Movimiento = (Guerrero) => (Guerrero) => Guerreros

  def DejarseFajar(atacante: Guerrero)(oponente: Guerrero): Guerreros = {
    atacante.estado match {
      case Luchando => (atacante estas Fajado(1), oponente)
      case Fajado(rounds) => (atacante estas Fajado(rounds + 1), oponente)
      case _ => (atacante, oponente)
    }
  }

  def CargarKi(atacante: Guerrero)(oponente: Guerrero): Guerreros = {
    atacante.especie match {
      case Saiyajin(SuperSaiyajin(nivel), _) => (atacante estas Luchando actualizarEnergia (150 * nivel), oponente)
      case Androide => (atacante estas Luchando, oponente)
      case _ => (atacante estas Luchando actualizarEnergia 100, oponente)
    }
  }

  def MuchosGolpesNinja(ejecutante: Guerrero)(defensor: Guerrero): Guerreros = {
    val (atacante, oponente) = (ejecutante estas Luchando, defensor)
    (atacante.especie, oponente.especie) match {
      case (Humano, Androide) => (atacante actualizarEnergia -10, oponente)
      case _ => if (atacante.energia >= oponente.energia) (atacante, oponente actualizarEnergia -20) else (atacante actualizarEnergia -20, oponente)
    }
  }

  def Explotar(ejecutante: Guerrero)(defensor: Guerrero): Guerreros = {
    def _explotar(danio: Int, oponente: Guerrero): Guerrero = {
      val danioRecibido = oponente.energia - danio
      oponente.especie match {
        case Namekusein if danioRecibido <= 0 => oponente actualizarEnergia -(oponente.energia-1)
        case _ => oponente actualizarEnergia -Math.abs(danioRecibido)
      }
    }

    val (atacante, oponente) = (ejecutante estas Luchando, defensor)
    val factor = if (atacante.especie == Androide) 3 else 2
    atacante.especie match {
      case Androide /*| Monstruo(_,_)*/ => (atacante Matate, _explotar(atacante.energia*factor, oponente))
      case _ => (atacante, oponente)
    }
  }

  def Onda(energiaNecesaria: Int)(ejecutante: Guerrero)(defensor: Guerrero): Guerreros = {
    val (atacante, oponente) = (ejecutante estas Luchando, defensor)
    if (atacante.energia < energiaNecesaria) (atacante, oponente)
    else oponente.especie match {
//      case Monstruo(_,_) => (-energiaNecesaria, -(energiaNecesaria / 2))
      case Androide => (atacante actualizarEnergia -energiaNecesaria, oponente actualizarEnergia Math.abs(energiaNecesaria * 2))
      case _ => (atacante actualizarEnergia -energiaNecesaria, oponente actualizarEnergia -(energiaNecesaria * 2))
    }
  }

  def Genkidama(atacante: Guerrero)(oponente: Guerrero): Guerreros = {
    def calcularDanio(estado: Estado): Int = {
      estado match {
        case Fajado(rounds) => -Math.pow(10, rounds).toInt
        case _ => -10
      }
    }
    (atacante.estado, oponente.especie) match {
      case (_, Androide) => (atacante estas Luchando, oponente actualizarEnergia Math.abs(calcularDanio(atacante.estado)))
      case (_,_) => (atacante estas Luchando, oponente actualizarEnergia calcularDanio(atacante.estado))
    }
  }

  abstract class MovimientoDeprecated {
    def movimiento(guerreros: Guerreros): Guerreros
    def apply(guerreros: Guerreros) = {
      val (atacante, oponente) = guerreros
      (atacante.estado, this) match {
        case (Muerto, _) => guerreros
        case (KO, UsarItem(SemillaDelErmitanio)) => movimiento(guerreros)
        case (KO, _) => guerreros
        case (Luchando, _) => movimiento(guerreros)
//        case (Fajado(_), Genkidama) => movimiento(guerreros)
        case (Fajado(_), _) => movimiento(atacante estas Luchando, oponente)
      }
    }
  }


//  case object ComerseAlOponente extends MovimientoDeprecated {
//    override def movimiento(guerreros: Guerreros) = {
//      val (atacante, oponente) = guerreros
//      atacante.especie match {
//        case Monstruo(tipoDigestion, guerrerosComidos) if oponente.energia < atacante.energia =>
//          (atacante.comerseA(oponente, tipoDigestion, guerrerosComidos), oponente.estas(Muerto))
//        case _ => guerreros
//      }
//    }
//  }

  case class UsarItem(item: Item) extends MovimientoDeprecated {
    def quedarKOSiEnergiaMenorA300(guerrero: Guerrero) = if (guerrero.energia < 300) guerrero.estas(KO) else guerrero
    override def movimiento(guerreros: Guerreros) = {
      val (atacante, oponente) = guerreros
      (item, oponente.especie, oponente.estado) match {
        case (ArmaRoma, Androide, _) => (atacante, oponente)
        case (ArmaRoma, _, _) => (atacante, quedarKOSiEnergiaMenorA300(oponente))
        case (ArmaFilosa, Saiyajin(MonoGigante, _), _) =>
          (atacante, oponente.cambiarEspecieA(Saiyajin(Normal, cola = false)).cambiarEnergiaA(1).estas(KO))
        case (ArmaFilosa, Saiyajin(estado, tieneCola), _) if tieneCola =>
          (atacante, oponente.cambiarEspecieA(Saiyajin(estado, cola = false)).cambiarEnergiaA(1))
        case (ArmaFilosa, _, _) => (atacante, oponente actualizarEnergia -(atacante.energia / 100))
        //TODO: Restar una municion del inventario
        case (ArmaDeFuego, Humano, _) => (atacante, oponente actualizarEnergia -20)
        case (ArmaDeFuego, Namekusein, KO) => (atacante, oponente actualizarEnergia -10)
        case (SemillaDelErmitanio, _, _) => (atacante.recuperarEnergiaMaxima.eliminarItem(item), oponente)
        case (_,_,_) => (atacante, oponente)
      }
    }
  }
//
//  case object ConvertirseEnMono extends MovimientoDeprecated {
//    override def movimiento(guerreros: Guerreros) = {
//      val (atacante, oponente) = guerreros
//      atacante.especie match {
//        case Saiyajin(MonoGigante, _) => (atacante, oponente)
//        case Saiyajin(_, tieneCola) if tieneCola && atacante.tieneItem(FotoDeLaLuna) =>
//          (atacante.cambiarEstadoSaiyajin(MonoGigante,tieneCola).recuperarEnergiaMaxima.multiplicarEnergiaMaximaPor(3), oponente)
//        case _ => (atacante, oponente)
//      }
//    }
//  }
//
//  case object ConvertirseEnSuperSaiyajin extends MovimientoDeprecated {
//    override def movimiento(guerreros: Guerreros) = {
//      val (atacante, oponente) = guerreros
//      atacante.especie match {
//        case Saiyajin(MonoGigante, _) => (atacante, oponente)
//        case Saiyajin(estado, tieneCola) if atacante.puedeSubirDeNivel() =>
//          (atacante.cambiarEstadoSaiyajin(SuperSaiyajin(estado.proximoNivelZ),tieneCola).multiplicarEnergiaMaximaPor(5), oponente)
//        case _ => (atacante, oponente)
//      }
//    }
//  }

//  case class Fusionarse(guerreroParaFusionar: Guerrero) extends MovimientoDeprecated  {
//    override def movimiento(guerreros: Guerreros) = {
//      val (amigo, oponente) = guerreros
//      (amigo.especie, guerreroParaFusionar.especie) match {
//        case (_:Fusionable, _:Fusionable) =>
//          val nuevoGuerrero: Guerrero =
//            guerreroParaFusionar.cambiarEnergiaMaximaA(amigo.energiaMaxima + amigo.energiaMaxima)
//              .cambiarEnergiaA(amigo.energia + guerreroParaFusionar.energia)
//              .agregarMovimientos(amigo.movimientosPropios)
//              .cambiarEspecieA(Fusion)
//          (nuevoGuerrero, oponente)
//        case _ => (guerreroParaFusionar, amigo)
//      }
//    }
//  }

//  case class Magia(paseDeMagia: Guerreros => Guerreros) extends MovimientoDeprecated {
//    override def movimiento(guerrreros: Guerreros) = {
//      val(atacante, oponente): Guerreros = guerrreros
//      atacante.especie match {
//       case _:Magico => paseDeMagia(guerrreros)
//       case _ if (atacante.tieneLas7Esferas) => paseDeMagia (atacante.eliminarEsferas, oponente)
//       case _ => guerrreros
//     }
//   }
//  }
}
