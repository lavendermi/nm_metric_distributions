package ca.mikelavender.nm_metric_evaluations

import org.apache.commons.compress.archivers.{ArchiveEntry, ArchiveInputStream, ArchiveStreamFactory}
import org.apache.commons.compress.compressors.{CompressorInputStream, CompressorStreamFactory}
import org.apache.commons.io.input.CloseShieldInputStream

import java.io.{BufferedInputStream, InputStream}

object Unpacker {

  /** Unpack a compressed archive from an input stream; for example, a stream of
   * bytes from a tar.gz or tar.xz archive.
   *
   * The result is an iterator of 2-tuples, one for each entry in the archive:
   *
   *   - An ArchiveEntry instance, with information like name, size and whether
   * the entry is a file or directory
   *   - An InputStream of all the bytes in this particular entry
   *
   */
  def open(inputStream: InputStream): Iterator[(ArchiveEntry, InputStream)] = {
//    for {
      val uncompressedInputStream = createUncompressedStream(inputStream)
      val archiveInputStream = createArchiveStream(uncompressedInputStream)
      createIterator(archiveInputStream)
    } //yield
//iterator

  private def createUncompressedStream(inputStream: InputStream) = {
    new CompressorStreamFactory().createCompressorInputStream(
      getMarkableStream(inputStream)
    )
  }

  private def createArchiveStream(uncompressedInputStream: CompressorInputStream) = {
    new ArchiveStreamFactory()
      .createArchiveInputStream(
        getMarkableStream(uncompressedInputStream)
      )
  }

  private def createIterator(
                              archiveInputStream: ArchiveInputStream): Iterator[(ArchiveEntry, InputStream)] =
    new Iterator[(ArchiveEntry, InputStream)] {
      var latestEntry: ArchiveEntry = _

      override def hasNext: Boolean = {
        latestEntry = archiveInputStream.getNextEntry
        latestEntry != null
      }

      override def next(): (ArchiveEntry, InputStream) =
        (latestEntry, new CloseShieldInputStream(archiveInputStream))
    }

  private def getMarkableStream(inputStream: InputStream): InputStream =
    if (inputStream.markSupported())
      inputStream
    else
      new BufferedInputStream(inputStream)
}