#!/usr/bin/env python3
from __future__ import annotations
import csv
import hashlib
import re
import sys
import time
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ET
from pathlib import Path
from typing import Iterable

CONSULTA = (
    '("queueing theory"[Title/Abstract] OR '
    '"queuing theory"[Title/Abstract]) AND '
    '("2000/01/01"[Date - Publication] : '
    '"2017/12/31"[Date - Publication])'
)

HUELLA_PMIDS_ORIGINALES = (
    "addbdbe60c458c72b62ed6f89fda6ed730a6ec82ba263561e815e8907627c955"
)

PMIDS_FUERA_DE_ALCANCE = frozenset(
    {
        "10918622", "11032032", "11408979", "16574335", "16595337",
        "16766611", "16849207", "16982070", "17046773", "17328218",
        "17367335", "19233215", "20365433", "20524149", "20866831",
        "21077709", "21405439", "21728599", "22042124", "22102802",
        "22181081", "22186735", "22311253", "22408506", "22463184",
        "22996722", "23005146", "23126453", "23150569", "23230164",
        "24428386", "24917169", "24982982", "25027996", "25142745",
        "25207992", "25622323", "25759720", "26474290", "26743048",
        "26982532", "27069412", "27100335", "27387090", "27563896",
        "28374484", "28425119", "28535148", "29236698",
    }
)

CAMPOS_CSV = [
    "UT", "PMID", "AU", "AF", "TI", "SO", "J9", "PY", "DT", "DE", "ID",
    "DI", "AB", "LA", "C1", "VL", "IS", "BP", "URL", "DB", "TC", "CR",
]


def texto(elemento: ET.Element | None) -> str:
    """Devuelve todo el texto interno de un elemento XML."""
    return "" if elemento is None else "".join(elemento.itertext()).strip()


def obtener(url: str, intentos: int = 5) -> bytes:
    """Descarga una URL con reintentos breves."""
    solicitud = urllib.request.Request(
        url, headers={"User-Agent": "Bibliometria-Colas-Salud/1.0"}
    )
    for intento in range(1, intentos + 1):
        try:
            with urllib.request.urlopen(solicitud, timeout=60) as respuesta:
                return respuesta.read()
        except Exception:
            if intento == intentos:
                raise
            time.sleep(2 * intento)


def extraer_pmids_del_txt(pubmed_txt: Path) -> list[str]:
    """Extrae y valida los PMID de la exportación original de PubMed."""
    contenido = pubmed_txt.read_text(encoding="utf-8-sig")
    pmids = re.findall(r"^PMID-\s*(\d+)\s*$", contenido, flags=re.MULTILINE)
    if not pmids:
        raise ValueError(f"{pubmed_txt} no contiene líneas PMID-.")
    if len(pmids) != len(set(pmids)):
        raise ValueError(f"{pubmed_txt} contiene PMID repetidos.")

    canonico = "\n".join(sorted(pmids, key=int)) + "\n"
    huella = hashlib.sha256(canonico.encode("ascii")).hexdigest()
    if len(pmids) != 133 or huella != HUELLA_PMIDS_ORIGINALES:
        raise RuntimeError(
            "PubMed_Original.txt no contiene el conjunto histórico exacto de "
            "133 PMID utilizado en el informe."
        )
    return pmids


def descargar_xml_por_pmids(pmids: list[str], anexos: Path) -> list[Path]:
    """Descarga con EFetch los PMID y sobrescribe los XML esperados."""
    anexos.mkdir(parents=True, exist_ok=True)
    descargas: list[tuple[Path, bytes]] = []

    for inicio in range(0, len(pmids), 100):
        lote = pmids[inicio : inicio + 100]
        parametros = urllib.parse.urlencode(
            {
                "db": "pubmed",
                "id": ",".join(lote),
                "retmode": "xml",
                "tool": "bibliometria_colas_salud",
            }
        )
        url = (
            "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?"
            + parametros
        )
        destino = anexos / f"pubmed_{inicio}.xml"
        datos = obtener(url)

        try:
            raiz = ET.fromstring(datos)
        except ET.ParseError as error:
            raise RuntimeError(
                f"PubMed devolvió un XML no válido para el lote {inicio}: {error}"
            ) from error
        cantidad = len(raiz.findall(".//PubmedArticle"))
        if cantidad != len(lote):
            raise RuntimeError(
                f"El lote {inicio} debía contener {len(lote)} registros y contiene "
                f"{cantidad}."
            )

        descargas.append((destino, datos))
        time.sleep(0.4)

    for destino, datos in descargas:
        destino.write_bytes(datos)
    (anexos / "Ecuacion_Busqueda.txt").write_text(
        CONSULTA + "\n", encoding="utf-8"
    )
    print(f"PMID extraídos de PubMed_Original.txt: {len(pmids)}")
    print(f"XML descargados y sobrescritos en: {anexos.resolve()}")
    return [destino for destino, _ in descargas]


def cargar_articulos(archivos_xml: Iterable[Path]) -> dict[str, ET.Element]:
    """Carga los artículos y verifica que cada PMID aparezca una sola vez."""
    articulos: dict[str, ET.Element] = {}
    for archivo in archivos_xml:
        try:
            raiz = ET.parse(archivo).getroot()
        except ET.ParseError as error:
            raise ValueError(f"El XML {archivo} no es válido: {error}") from error
        encontrados = raiz.findall(".//PubmedArticle")
        if not encontrados:
            raise ValueError(f"{archivo} no contiene registros PubmedArticle.")
        for articulo in encontrados:
            pmid = texto(articulo.find("./MedlineCitation/PMID"))
            if not pmid:
                raise ValueError(f"Se encontró un registro sin PMID en {archivo}.")
            if pmid in articulos:
                raise ValueError(f"El PMID {pmid} aparece en más de un XML.")
            articulos[pmid] = articulo
    return articulos


def cargar_decisiones(registro: Path) -> dict[str, dict[str, str]]:
    """Lee las decisiones de inclusión y exclusión documentadas."""
    decisiones: dict[str, dict[str, str]] = {}
    with registro.open("r", encoding="utf-8-sig", newline="") as archivo:
        lector = csv.DictReader(archivo)
        for fila in lector:
            pmid = fila["PMID"].strip()
            decision = fila["Decision"].strip().casefold()
            if decision not in {"incluir", "excluir"}:
                raise ValueError(f"Decisión no reconocida para PMID {pmid}.")
            if pmid in decisiones:
                raise ValueError(f"El PMID {pmid} está repetido en {registro}.")
            decisiones[pmid] = fila
    return decisiones


def extraer_anio(articulo_xml: ET.Element, pmid: str) -> int:
    """Obtiene el año del fascículo, igual que en la base entregada."""
    articulo = articulo_xml.find("./MedlineCitation/Article")
    if articulo is None:
        raise ValueError(f"El PMID {pmid} no contiene Article.")
    fecha = articulo.find("Journal/JournalIssue/PubDate")
    anio = texto(fecha.find("Year")) if fecha is not None else ""
    if not anio and fecha is not None:
        coincidencia = re.search(r"\d{4}", texto(fecha.find("MedlineDate")))
        anio = coincidencia.group(0) if coincidencia else ""
    if not anio:
        raise ValueError(f"No se pudo determinar el año del PMID {pmid}.")
    return int(anio)


def extraer_fila(articulo_xml: ET.Element) -> dict[str, str | int]:
    """Convierte un PubmedArticle en una fila de la matriz bibliográfica."""
    pmid = texto(articulo_xml.find("./MedlineCitation/PMID"))
    articulo = articulo_xml.find("./MedlineCitation/Article")
    if articulo is None:
        raise ValueError(f"El PMID {pmid} no contiene Article.")

    autores_abreviados: list[str] = []
    autores_completos: list[str] = []
    for autor in articulo.findall("AuthorList/Author"):
        apellido = texto(autor.find("LastName"))
        iniciales = texto(autor.find("Initials"))
        nombre = texto(autor.find("ForeName")) or iniciales
        colectivo = texto(autor.find("CollectiveName"))
        if apellido:
            autores_abreviados.append(f"{apellido.upper()} {iniciales.upper()}")
            autores_completos.append(f"{apellido}, {nombre}")
        elif colectivo:
            autores_abreviados.append(colectivo.upper())
            autores_completos.append("{" + colectivo + "}")

    doi = ""
    for identificador in articulo_xml.findall(".//ArticleId"):
        if identificador.attrib.get("IdType") == "doi":
            doi = texto(identificador)
            break

    pagina_inicial = texto(articulo.find("Pagination/StartPage"))
    pagina_final = texto(articulo.find("Pagination/EndPage"))
    paginas_medline = texto(articulo.find("Pagination/MedlinePgn"))
    paginas = (
        f"{pagina_inicial}-{pagina_final}"
        if pagina_inicial and pagina_final
        else pagina_inicial or paginas_medline
    )

    return {
        "UT": f"PUBMED:{pmid}",
        "PMID": pmid,
        "AU": ";".join(autores_abreviados),
        "AF": ";".join(autores_completos),
        "TI": texto(articulo.find("ArticleTitle")),
        "SO": texto(articulo.find("Journal/Title")),
        "J9": texto(articulo.find("Journal/ISOAbbreviation")),
        "PY": extraer_anio(articulo_xml, pmid),
        "DT": ";".join(
            texto(x) for x in articulo.findall("PublicationTypeList/PublicationType")
        ),
        "DE": ";".join(texto(x) for x in articulo_xml.findall(".//Keyword")),
        "ID": ";".join(
            texto(x) for x in articulo_xml.findall(".//MeshHeading/DescriptorName")
        ),
        "DI": doi,
        "AB": " ".join(texto(x) for x in articulo.findall("Abstract/AbstractText")),
        "LA": ";".join(texto(x) for x in articulo.findall("Language")),
        "C1": ";".join(texto(x) for x in articulo_xml.findall(".//Affiliation")),
        "VL": texto(articulo.find("Journal/JournalIssue/Volume")),
        "IS": texto(articulo.find("Journal/JournalIssue/Issue")),
        "BP": paginas,
        "URL": f"https://pubmed.ncbi.nlm.nih.gov/{pmid}/",
        "DB": "PUBMED",
        "TC": "",
        "CR": "",
    }


def generar_registro_depuracion(
    articulos: dict[str, ET.Element], registro: Path
) -> None:
    """Reconstruye las decisiones documentadas para los 133 PMID originales."""
    filas: list[dict[str, str]] = []
    for pmid, articulo_xml in articulos.items():
        articulo = articulo_xml.find("./MedlineCitation/Article")
        if articulo is None:
            raise ValueError(f"El PMID {pmid} no contiene Article.")
        titulo = texto(articulo.find("ArticleTitle"))
        tiene_autor = any(
            texto(autor.find("LastName")) or texto(autor.find("CollectiveName"))
            for autor in articulo.findall("AuthorList/Author")
        )
        anio = extraer_anio(articulo_xml, pmid)

        if pmid in PMIDS_FUERA_DE_ALCANCE:
            decision = "Excluir"
            motivo = "Fuera de alcance: no estudia operaciones de servicios de salud"
        elif pmid == "19305770":
            decision = "Excluir"
            motivo = "Duplicado de título y autores; se conserva PMID 18536537"
        elif not tiene_autor:
            decision = "Excluir"
            motivo = "Registro sin autor identificado"
        elif anio > 2017:
            decision = "Excluir"
            motivo = (
                "Año del fascículo posterior a 2017; recuperado por fecha "
                "electrónica anterior"
            )
        else:
            decision = "Incluir"
            motivo = "Pertinente: servicios de salud; artículo/revisión; periodo válido"
        filas.append(
            {"PMID": pmid, "Titulo": titulo, "Decision": decision, "Motivo": motivo}
        )

    incluidos = sum(fila["Decision"] == "Incluir" for fila in filas)
    if len(filas) != 133 or incluidos != 77:
        raise RuntimeError(
            "La depuración reconstruida no coincide con los conteos esperados."
        )

    with registro.open("w", encoding="utf-8", newline="") as archivo:
        escritor = csv.DictWriter(
            archivo, fieldnames=["PMID", "Titulo", "Decision", "Motivo"]
        )
        escritor.writeheader()
        escritor.writerows(filas)
    print(f"Registro de depuración sobrescrito: {registro.resolve()}")


def escapar_bibtex(valor: str) -> str:
    """Escapa caracteres con significado especial en BibTeX/LaTeX."""
    for original, reemplazo in (
        ("&", r"\&"), ("%", r"\%"), ("_", r"\_"), ("#", r"\#")
    ):
        valor = valor.replace(original, reemplazo)
    return valor


def crear_bibtex(filas: list[dict[str, str | int]]) -> str:
    """Crea una entrada @article por cada fila aceptada."""
    entradas: list[str] = []
    for fila in filas:
        autores = " and ".join(
            escapar_bibtex(autor) for autor in str(fila["AF"]).split(";") if autor
        )
        campos = {
            "author": autores,
            "title": "{" + escapar_bibtex(str(fila["TI"])) + "}",
            "journal": escapar_bibtex(str(fila["SO"])),
            "year": str(fila["PY"]),
            "volume": str(fila["VL"]),
            "number": str(fila["IS"]),
            "pages": str(fila["BP"]),
            "doi": str(fila["DI"]),
            "url": str(fila["URL"]),
            "keywords": escapar_bibtex(str(fila["DE"])),
            "mesh": escapar_bibtex(str(fila["ID"])),
            "pmid": str(fila["PMID"]),
        }
        cuerpo = ",\n".join(
            f"  {nombre} = {{{valor}}}" for nombre, valor in campos.items() if valor
        )
        entradas.append(f"@article{{PMID{fila['PMID']},\n{cuerpo}\n}}")
    return "\n\n".join(entradas)


def generar_bases(
    archivos_xml: list[Path], registro: Path, salida: Path
) -> None:
    """Genera el CSV y el BibTeX a partir de los XML y las decisiones."""
    articulos = cargar_articulos(archivos_xml)
    decisiones = cargar_decisiones(registro)
    pmids_xml = set(articulos)
    pmids_registro = set(decisiones)
    if pmids_xml != pmids_registro:
        raise RuntimeError(
            "Los XML y Registro_Depuracion.csv no describen el mismo conjunto."
        )

    incluidos = {
        pmid
        for pmid, fila in decisiones.items()
        if fila["Decision"].strip().casefold() == "incluir"
    }
    filas = [extraer_fila(articulos[pmid]) for pmid in incluidos]
    filas.sort(key=lambda fila: (int(fila["PY"]), int(fila["PMID"])))
    if len(articulos) != 133 or len(filas) != 77:
        raise RuntimeError("No se obtuvieron los 133 registros ni los 77 incluidos.")

    ruta_csv = salida / "Base_Datos_Bibliografica.csv"
    ruta_bib = salida / "Base_Datos_Bibliografica.bib"
    with ruta_csv.open("w", encoding="utf-8", newline="") as archivo:
        escritor = csv.DictWriter(archivo, fieldnames=CAMPOS_CSV)
        escritor.writeheader()
        escritor.writerows(filas)
    ruta_bib.write_text(crear_bibtex(filas), encoding="utf-8")

    print(f"XML procesados: {len(archivos_xml)}")
    print(f"Registros originales: {len(articulos)}")
    print(f"Registros incluidos: {len(filas)}")
    print(f"CSV generado: {ruta_csv.resolve()}")
    print(f"BibTeX generado: {ruta_bib.resolve()}")


def main() -> int:
    anexos = Path(__file__).resolve().parent
    proyecto = anexos.parent
    try:
        pmids = extraer_pmids_del_txt(anexos / "PubMed_Original.txt")
        archivos_xml = descargar_xml_por_pmids(pmids, anexos)
        articulos = cargar_articulos(archivos_xml)
        registro = anexos / "Registro_Depuracion.csv"
        generar_registro_depuracion(articulos, registro)
        generar_bases(archivos_xml, registro, proyecto)
        return 0
    except Exception as error:
        print(f"ERROR: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
