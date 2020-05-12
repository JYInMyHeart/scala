package poem
import java.io.{File, PrintWriter}

import com.alibaba.fastjson.{JSON, JSONObject}

import scala.io.Source

object Poem extends App {

  def readFile(): Unit = {
    val root = new File("D:\\poem\\poetry\\data\\poetry")
    val sqls = root
      .listFiles()
      .map(f => {
        val source = Source.fromFile(f)
        val jsonStr = source.getLines().foldLeft("")(_ + _)
        val jsonObject = JSON.parseObject(jsonStr)
        val content = jsonObject.getString("content")
        val dynasty = jsonObject.getString("dynasty")
        val translate = jsonObject.getString("fanyi")
        val id = jsonObject.getString("id")
        val title = jsonObject.getString("name")
        val author = dynasty + " " + jsonObject
          .getJSONObject("poet")
          .getString("name")
        val comment = jsonObject.getString("shangxi")
        val likeNum = jsonObject.getString("star")
        val tags = jsonObject.getJSONArray("tags")
        val buffer = new StringBuilder()
        for (i <- 0 until tags.size()) {
          buffer.append(tags.getString(i)).append(",")
        }
        var label = ""
        if (buffer.nonEmpty) {
          label = buffer.toString().substring(0, buffer.length - 1)
        }
        val sql =
          s"insert into poem (poem_title, poem_author, poem_content, poem_label, poem_like, poem_comment, poem_trans_content, poem_is_like) values ('$title','$author','$content','$label','$likeNum','$comment','$translate','false')"

        sql
      })
      .filter(s => !s.contains("null"))
      .filter(s => s.contains("唐"))
      .fold("")(_ + ";\r\n" + _)
    val writer = new PrintWriter(new File("test.txt"))

    writer.write(sqls)
    writer.close()
  }
  readFile()

}
