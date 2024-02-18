package ca.mikelavender.nm_metric_evaluations

import org.apache.commons.compress.archivers.ArchiveException
import org.apache.commons.compress.compressors.CompressorException

sealed trait UnarchiverError {
  val e: Throwable
}

case class CompressorError(e: CompressorException) extends UnarchiverError
case class ArchiveFormatError(e: ArchiveException) extends UnarchiverError
case class UnexpectedUnarchiverError(e: Throwable) extends UnarchiverError
