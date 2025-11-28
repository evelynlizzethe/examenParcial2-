/*
Estoy realizando un examen, es de programacion en Scala, el ejercicio dice: En una editorial universitaria
se dispone de una lista de libros ya modelada en Scala mediante:
case class Libro(titulo: String, autor: String, paginas: Int, anio: Int)
val catalogo: List[Libro] = List(
  Libro("Programación en Scala", "Ana Ruiz", 150, 2010),
  Libro("Fundamentos de FP", "Juan Pérez", 165, 2011),
  Libro("Algoritmos Modernos", "Carlos León", 180, 2012),
  Libro("Estructuras de Datos", "María Gómez", 195, 2013),
  Libro("Introducción a la Programación", "Luis Andrade", 210, 2014),
  Libro("Técnicas de Depuración", "Ana Ruiz", 225, 2015),
  Libro("Diseño de Sistemas", "Juan Pérez", 240, 2016),
  Libro("Patrones de Diseño", "Carlos León", 255, 2017),
  Libro("Aplicaciones Web", "María Gómez", 270, 2018),
  Libro("Cómputo en la Nube", "Luis Andrade", 285, 2019),
  Libro("Arquitectura de Software", "Ana Ruiz", 300, 2015),
  Libro("Bases de Datos", "Juan Pérez", 315, 2016),
  Libro("Microservicios", "Carlos León", 330, 2017),
  Libro("Concurrencia en Java", "María Gómez", 345, 2018),
  Libro("Pruebas Automatizadas", "Luis Andrade", 360, 2019),
  Libro("Seguridad Aplicada", "Ana Ruiz", 375, 2016),
  Libro("DevOps Práctico", "Juan Pérez", 390, 2017),
  Libro("Análisis de Datos", "Carlos León", 405, 2018),
  Libro("Machine Learning Básico", "María Gómez", 420, 2019),
  Libro("Redes de Computadores", "Luis Andrade", 435, 2019)
)
El área académica quiere identificar al autor más productivo, considerando únicamente los libros que tengan más de
cierto número mínimo de páginas y que hayan sido publicados a partir de un año dado.
 */

case class Libro(
                  titulo: String,
                  autor: String,
                  paginas: Int,
                  anio: Int
                )
case class AutorInfo(
                      autor: String,
                      totalPaginas: Int,
                      cantidadLibros: Int)
val catalogo: List[Libro] = List(
  Libro("Programación en Scala", "Ana Ruiz", 150, 2010),
  Libro("Fundamentos de FP", "Juan Pérez", 165, 2011),
  Libro("Algoritmos Modernos", "Carlos León", 180, 2012),
  Libro("Estructuras de Datos", "María Gómez", 195, 2013),
  Libro("Introducción a la Programación", "Luis Andrade", 210, 2014),
  Libro("Técnicas de Depuración", "Ana Ruiz", 225, 2015),
  Libro("Diseño de Sistemas", "Juan Pérez", 240, 2016),
  Libro("Patrones de Diseño", "Carlos León", 255, 2017),
  Libro("Aplicaciones Web", "María Gómez", 270, 2018),
  Libro("Cómputo en la Nube", "Luis Andrade", 285, 2019),
  Libro("Arquitectura de Software", "Ana Ruiz", 300, 2015),
  Libro("Bases de Datos", "Juan Pérez", 315, 2016),
  Libro("Microservicios", "Carlos León", 330, 2017),
  Libro("Concurrencia en Java", "María Gómez", 345, 2018),
  Libro("Pruebas Automatizadas", "Luis Andrade", 360, 2019),
  Libro("Seguridad Aplicada", "Ana Ruiz", 375, 2016),
  Libro("DevOps Práctico", "Juan Pérez", 390, 2017),
  Libro("Análisis de Datos", "Carlos León", 405, 2018),
  Libro("Machine Learning Básico", "María Gómez", 420, 2019),
  Libro("Redes de Computadores", "Luis Andrade", 435, 2019)
)

// Autor más productivo bajo los criterios indicados (mínimo de páginas y año mínimo)
def autorMasProductivo(catalogo: List[Libro], paginasMin: Int, anioMin: Int): Option[String] = {

  val filtrados = catalogo.filter(l => l.paginas > paginasMin && l.anio >= anioMin)

  val conteo = mutable.Map[String, Int]()

  for (libro <- filtrados) {
    val actual = conteo.getOrElse(libro.autor, 0)
    conteo(libro.autor) = actual + 1
  }

  if (conteo.isEmpty) None
  else Some(conteo.maxBy(_._2)._1)
}

// Tomar solo los libros que cumplen las condiciones (páginas y año)
var filtrados: List[Libro] = List()

for (libro <- catalogo) {
  if (libro.paginas > paginasMin && libro.anio >= anioMin) {
    filtrados = filtrados :+ libro
  }
}
// A partir de esos libros filtrados, obtener la lista de autores sin repetir.

var autores: List[String] = List()

for (libro <- filtrados) {
  if (!autores.contains(libro.autor)) {
    autores = autores :+ libro.autor
  }
}

// Para cada autor de esa lista, recorrer la colección de libros filtrados y calcular cuántas
// páginas suma y cuántos libros tiene.

var infoAutores: List[AutorInfo] = List()

for (autor <- autores) {
  var paginas = 0
  var libros = 0

  for (libro <- filtrados) {
    if (libro.autor == autor) {
      paginas += libro.paginas
      libros += 1
    }
  }

  val info = AutorInfo(autor, paginas, libros)

  infoAutores = infoAutores :+ info
}

// Construir un objeto AutorInfo para cada autor con esos datos.

var infoAutores: List[AutorInfo] = List()

for (autor <- autores) {
  var paginas = 0
  var libros = 0

  for (libro <- filtrados) {
    if (libro.autor == autor) {
      paginas += libro.paginas
      libros += 1
    }
  }

  val info = AutorInfo(autor, paginas, libros)
  infoAutores = infoAutores :+ info
}
// Recorrer la colección de AutorInfo y quedarse con aquel cuyo campo totalPaginas sea el mayor.

var mejor: Option[AutorInfo] = None

for (info <- infoAutores) {
  mejor match {
    case None =>
      mejor = Some(info)

    case Some(actual) =>
      if (info.totalPaginas > actual.totalPaginas) {
        mejor = Some(info)
      }
  }
}
