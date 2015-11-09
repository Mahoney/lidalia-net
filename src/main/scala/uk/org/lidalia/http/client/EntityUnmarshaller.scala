package uk.org.lidalia.http.client

import java.io.InputStream

import org.apache.commons.io.IOUtils
import uk.org.lidalia.http.core.{ByteEntity, EmptyEntity, Entity, Request, ResponseHeader}
import uk.org.lidalia.lang.ByteSeq

object NoopEntityUnmarshaller extends EntityUnmarshaller[None.type] {
  override def unmarshal(request: Request[None.type, _], response: ResponseHeader, entityBytes: InputStream) = EmptyEntity
}

object BytesUnmarshaller extends EntityUnmarshaller[ByteSeq] {
  override def unmarshal(request: Request[ByteSeq, _], response: ResponseHeader, entityBytes: InputStream): Entity[ByteSeq] = {
    new ByteEntity(ByteSeq(IOUtils.toByteArray(entityBytes)))
  }
}

trait EntityUnmarshaller[T] {

  def unmarshal(request: Request[T, _], response: ResponseHeader, entityBytes: InputStream): Entity[T]

}
