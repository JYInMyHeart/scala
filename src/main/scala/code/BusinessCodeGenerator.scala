package code

import java.io.{File, PrintWriter}

import scala.io.StdIn

object BusinessCodeGenerator {
  val newLine = "\r\n"

  def main(args: Array[String]): Unit = {
    val fileName = input
    actionFile(fileName)
    mallFile(fileName)
    reqFile(fileName)
    serviceFile(fileName)
    serviceImplFile(fileName)
    configFile(fileName)
  }

  def input = StdIn.readLine()

  def actionFile(fileName: String) = {
    val file = createActionFile(fileName)
    val writer = new PrintWriter(file)
    writeActionImport(writer)
    writeHead(writer, fileName)
    writeNewLine(writer)
    writeActionContent(writer, fileName)
    writeEnd(writer)
    closeWriter(writer)
  }

  def reqFile(fileName: String) = {
    val file = createReqFile(fileName)
    val writer = new PrintWriter(file)
    writeReqImport(writer)
    writeReqContent(writer, fileName)
    closeWriter(writer)
  }

  def serviceImplFile(fileName: String) = {
    val file = createServiceImplFile(fileName)
    val writer = new PrintWriter(file)
    writeServiceImplImport(writer, fileName)
    writeServiceImplContent(writer, fileName)
    closeWriter(writer)
  }

  def mallFile(fileName: String) = {
    val file = createMallFile(fileName)
    val writer = new PrintWriter(file)
    writeMallImport(writer)
    writeMallContent(writer, fileName)
    closeWriter(writer)
  }

  def configFile(fileName: String) = {
    val file = createConfigFile(fileName)
    val writer = new PrintWriter(file)
    writeConfigContent(writer, fileName)
    closeWriter(writer)
  }


  def writeServiceImplImport(writer: PrintWriter, fileName: String) = {
    writer.write("package cn.leadeon.thirdparty.service.impl;\n")
    writer.write("import cn.leadeon.thirdparty.base.ResultData;\nimport cn.leadeon.thirdparty.businesscode.BusinessCode;\nimport cn.leadeon.thirdparty.common.conf.WechatProConf;\nimport cn.leadeon.thirdparty.common.http.HttpClient;\nimport cn.leadeon.thirdparty.constant.ComVariable;\nimport cn.leadeon.thirdparty.constant.ThirdPartyServer;\nimport cn.leadeon.thirdparty.constant.ThridPartyResCode;\nimport cn.leadeon.thirdparty.log.Log;\nimport cn.leadeon.thirdparty.resultpojo.WechatMobileMallResultMall;")
    writer.write(s"import cn.leadeon.thirdparty.service.WXBaseThirdPartyService;\nimport cn.leadeon.thirdparty.service.${fileName}Service;\nimport com.alibaba.fastjson.JSON;\nimport com.alibaba.fastjson.JSONArray;\nimport com.alibaba.fastjson.JSONObject;\nimport com.ning.http.client.AsyncHttpClient;")
    writer.write("import org.springframework.beans.factory.annotation.Autowired;\nimport org.springframework.stereotype.Service;\n\nimport java.util.Map;")
    writer.write(s"import cn.leadeon.thirdparty.service.${fileName}Service;")
  }

  def writeServiceImplContent(writer: PrintWriter, fileName: String) = {
    writer.write(s"""@Service("${getServiceName(fileName)}")""")
    writeNewLine(writer)
    writer.write(s"public class ${fileName}ServiceImpl extends WXBaseThirdPartyService implements ${fileName}Service {")
    writer.write(s"private static final Log logger = new Log(${fileName}ServiceImpl.class);\n    private static final String busCode = BusinessCode.RECHARGE_WX_REMINDER;")
    writeNewLine(writer)
    writer.write("@Autowired\n    private WechatProConf wechatProConf;")
    writer.write(s" @Override\n    public ResultData<WechatMobileMallResultMall<${fileName}Mall>> get$fileName(String trace, Map<String, String> reqMap) {")
    writer.write(s"ResultData<WechatMobileMallResultMall<${fileName}Mall>> resObj;")
    writer.write("String resJson = \"\";\n        // 获取请求充值中心温馨提示url\n        String reqUrl = getUrl(reqMap);\n\n        // 请求商城数据打印\n        logger.reqPrint(Log.THIRDPARTY_SIGN, Log.THIRDPARTY_REQUEST, trace,\n                busCode, reqUrl);\n        Long startTime = System.currentTimeMillis();")
    writer.write("try {\n            AsyncHttpClient httpClient = new AsyncHttpClient(\n                    HttpClient.httpClientTimeOut());\n\n            // 请求商城\n            resJson = get(httpClient, ThirdPartyServer.RECHARGE_REMINDER, reqUrl);\n            logger.respPrint(Log.THIRDPARTY_SIGN, Log.THIRDPARTY_RESPONSE, trace,\n                    busCode, System.currentTimeMillis() - startTime, resJson);\n\n            // 商城响应json转为响应customer对象\n            resObj = resJsonToObj(trace, resJson);")
    writer.write("} catch (Exception e) {\n            logger.error(trace, e.getMessage(), e);\n            resObj = new ResultData<>();\n            resObj.setResultCode(ThridPartyResCode._0002);\n            resObj.setDesc(\"Request mobile mall error!\");\n            resObj.setException(e);\n        }")
    writer.write("// 商城返回数据打印\n        logger.respPrint(Log.THIRDPARTY_SIGN, Log.THIRDPARTY_RESPONSE, trace,\n                busCode, System.currentTimeMillis() - startTime, JSON.toJSONString(resObj));\n\n        return resObj;")
    writer.write("}")
    writer.write("private String getUrl(Map<String, String> reqMap) {\n       return null;}")
    writer.write(s"private ResultData<WechatMobileMallResultMall<${fileName}Mall>> resJsonToObj(\n            String trace, String resJson) {")
    writer.write(s"ResultData<WechatMobileMallResultMall<${fileName}Mall>> resObj = new ResultData<>();\n\n        WechatMobileMallResultMall<${fileName}Mall> mobileMallResObj = new WechatMobileMallResultMall<>();")
    writer.write("if (!StringUtils.isEmpty(resJson)) {\n            try {\n")
    writer.write("mobileMallResObj = resolveRespJson(resJson);")
    writer.write("resObj.setResultCode(ThridPartyResCode._0000);\n                resObj.setResultData(mobileMallResObj);")
    writer.write("} catch (Exception e) {\n                logger.error(\"trace:\" + trace + \" json to obj error，json =\" + resJson, e);\n                resObj.setResultCode(ThridPartyResCode._0002);\n                resObj.setDesc(\"Mobile mall response  data ERROR! mobileMallResJson=\" + resJson);\n                resObj.setException(e);\n                return resObj;\n            }\n        } else {\n            // 若http响应数据为空\n            logger.error(\"trace:\" + trace + \" json to obj error，json is empty\");\n            resObj.setResultCode(ThridPartyResCode._0001);\n            resObj.setDesc(\"Mobile mall ERROR!\");\n        }\n        return resObj;}")
    writer.write(s"private WechatMobileMallResultMall<${fileName}Mall> resolveRespJson(String resJson) {\n        return null;\n    }\n\n}")
  }


  def writeMallImport(writer: PrintWriter) = {
    writer.write("package cn.leadeon.thirdparty.resultpojo;\n")
    writer.write("import lombok.Data;\n\nimport java.io.Serializable;")
  }

  def writeMallContent(writer: PrintWriter, fileName: String) = {
    writeContent(writer, fileName)("Mall")
  }

  def writeContent(writer: PrintWriter, fileName: String)(suffix: String) = {
    writer.write("@Data")
    writeNewLine(writer)
    writer.write(s"public class ${fileName + suffix} implements Serializable{}")
  }


  def writeConfigContent(writer: PrintWriter, fileName: String) = {
    val serviceName = getServiceName(fileName)
    writer.write(
      s"""<!-- 微信小程序充值交费订单保存 -->
         |        <dubbo:reference id="$serviceName" interface="cn.leadeon.thirdparty.service.${fileName}Service" timeout="28000" />""".stripMargin)
    writeNewLine(writer)
    writer.write(
      s"""<!-- 微信小程序充值交费订单保存 -->
         |    <dubbo:service interface="cn.leadeon.thirdparty.service.${fileName}Service" ref="$serviceName"/>""".stripMargin)
  }


  def writeServiceImport(writer: PrintWriter, fileName: String) = {
    writer.write("package cn.leadeon.thirdparty.service;\n")
    writer.write("import cn.leadeon.thirdparty.base.ResultData;\nimport cn.leadeon.thirdparty.resultpojo.WechatMobileMallResultMall;")
    writer.write(s"import cn.leadeon.thirdparty.resultpojo.${fileName}Mall;import java.util.Map;")
  }

  def writeServiceContent(writer: PrintWriter, fileName: String) = {
    writer.write(s"public interface ${fileName}Service {")
    writeNewLine(writer)
    writer.write(s"    ResultData<WechatMobileMallResultMall<${fileName}Mall>> get$fileName(String trace, Map<String, String> reqMap);\n}")
  }

  def serviceFile(fileName: String) = {
    val file = createServiceFile(fileName)
    val writer = new PrintWriter(file)
    writeServiceImport(writer, fileName)
    writeServiceContent(writer, fileName)
    closeWriter(writer)
  }

  def writeReqImport(writer: PrintWriter) = {
    writer.write("package cn.leadeon.thirdparty.reqbody;\n")
    writer.write("import cn.leadeon.thirdparty.paramcheck.annotion.StrVerify;\nimport lombok.Data;\n\nimport java.io.Serializable;")
  }

  def writeReqContent(writer: PrintWriter, fileName: String) = {
    writeContent(writer, fileName)("Req")
  }


  def createFile(fileName: String)(suffix: String): File = {
    val file = new File(s"${fileName + suffix}")
    if (!file.exists()) {
      file.createNewFile()
    }
    file
  }

  def createActionFile(fileName: String) = createFile(fileName)("Action.java")

  def createMallFile(fileName: String) = createFile(fileName)("Mall.java")

  def createReqFile(fileName: String) = createFile(fileName)("Req.java")

  def createServiceFile(fileName: String) = createFile(fileName)("Service.java")

  def createServiceImplFile(fileName: String) = createFile(fileName)("ServiceImpl.java")

  def createConfigFile(fileName: String) = createFile(fileName)("config.txt")

  def writeActionImport(writer: PrintWriter) = {
    writer.write("package cn.leadeon.thirdparty.action;\n")
    writer.write("import cn.leadeon.thirdparty.base.ReqBody;")
    writer.write("import javax.servlet.http.HttpServletRequest;")
    writer.write("import cn.leadeon.thirdparty.base.ResBody;")
    writer.write("import cn.leadeon.thirdparty.base.ResultData;")
    writer.write("import cn.leadeon.thirdparty.businesscode.BusinessCode;")
    writer.write("import cn.leadeon.thirdparty.constant.ComVariable;")
    writer.write("import cn.leadeon.thirdparty.constant.ThridPartyResCode;")
    writer.write("import cn.leadeon.thirdparty.log.Log;")
    writer.write("import cn.leadeon.thirdparty.paramcheck.ValidationTool;")
    writer.write("import cn.leadeon.thirdparty.paramcheck.codemapping.ResponseCode;")
    writer.write("import cn.leadeon.thirdparty.paramcheck.codemapping.CodeMappingParam;")
    writer.write("import org.springframework.beans.factory.annotation.Autowired;")
    writer.write("import org.springframework.web.bind.annotation.PostMapping;")
    writer.write("import org.springframework.web.bind.annotation.RequestBody;")
    writer.write("import org.springframework.web.bind.annotation.RequestMapping;")
    writer.write("import org.springframework.web.bind.annotation.RestController;")
    writer.write("import com.alibaba.fastjson.JSON;\nimport com.alibaba.fastjson.TypeReference;")
    writer.write("import cn.leadeon.thirdparty.resultpojo.*;")
    writer.write("import cn.leadeon.thirdparty.service.*;")
    writer.write("import java.util.Map;")
    writer.write("import java.util.HashMap;")
  }

  def writeHead(writer: PrintWriter, fileName: String) = {
    writer.write("@RestController\n@RequestMapping(value = \"CHWX/reminderWXService\")")
    writer.write(s"public class ${fileName.concat("Action")} {")
  }

  def writeNewLine(writer: PrintWriter) = writer.write(newLine)

  def writeEnd(writer: PrintWriter) = {
    writeNewLine(writer)
    writer.write("}")
  }

  def closeWriter(writer: PrintWriter) = writer.close()

  def writeActionContent(writer: PrintWriter, fileName: String) = {
    writer.write(s"private static final Log logger = new Log(${fileName.concat("Action")}.class);")
    writer.write("private static final String busCode = BusinessCode.RECHARGE_WX_REMINDER;")
    writeNewLine(writer)
    writer.write("@Autowired")
    writeNewLine(writer)
    val service = fileName.concat("Service")
    val serviceName = getServiceName(fileName)
    writer.write(s"private ${service} ${serviceName};")
    writeNewLine(writer)
    writer.write("@PostMapping(value = \"/getWXReminder\")")
    writer.write(s"public ResBody<${fileName}Mall> get${fileName}(HttpServletRequest request, @RequestBody String reqData) {")
    writeNewLine(writer)
    writeMethod(writer, fileName)
  }

  def getServiceName(fileName: String) = {
    val service = fileName.concat("Service")
    service.charAt(0).toString.toLowerCase().concat(service.substring(1))
  }

  def writeMethod(writer: PrintWriter, fileName: String) = {
    val mall = fileName.concat("Mall")
    val serviceName = getServiceName(fileName)
    writer.write(s"// 定义返回结果对象\n        ResBody<$mall> resObj;\n        // 获取流水号，追踪日志使用\n        String trace = request.getHeader(ComVariable.TRACE);\n        // 客户端请求参数打印\n        logger.reqPrint(Log.THIRDPARTY_SIGN, Log.CLIENT_REQUEST, trace, busCode, reqData);\n        long startTime = System.currentTimeMillis();\n        try {\n            // 获取查询数据\n            resObj = getQueryData(trace, reqData);\n        } catch (Exception e) {\n            logger.error(trace, e);\n            resObj = new ResBody<>();\n            resObj.setRetCode(ResponseCode.SERVER_FAILURE);\n            resObj.setRetDesc(ComVariable.EXCEPTION_RESP_DESC);\n        }\n\n        // 响应客户端参数打印\n        logger.respPrint(Log.THIRDPARTY_SIGN, Log.CLIENT_RESPONSE, trace, busCode,\n                System.currentTimeMillis() - startTime, JSON.toJSONString(resObj));\n        return resObj;}")
    writer.write(s"private ResBody<${mall}> getQueryData(String trace, String reqData) {")
    writer.write(s"ResBody<${mall}> resObj = new ResBody<>();")
    val req = fileName.concat("Req")
    writer.write(s"// 请求参数获取\n        ReqBody<${req}> reqObj = null;")
    writer.write(s"try {\n            reqObj = JSON.parseObject(reqData, new TypeReference<ReqBody<${req}>>() {\n            });\n        } catch (Exception e) {\n            resObj.setRetCode(ResponseCode.REQUEST_ERROR);\n            resObj.setRetDesc(ComVariable.EXCEPTION_RESP_DESC);\n            return resObj;\n        }")
    writer.write("// 参数校验\n        CodeMappingParam codeMapping = ValidationTool.validationField(reqObj.getReqBody());\n        resObj.setRetCode(codeMapping.getCodeNumber());")
    writer.write("// 根据参数校验结果分别处理\n        if (ResponseCode.REQUEST_SUCCESS.equals(resObj.getRetCode())) {")
    writeNewLine(writer)
    writer.write(s"//             设置调用provider参数\n            Map<String, String> reqMap = get${fileName}Params(reqObj);")
    writer.write(s"ResultData<WechatMobileMallResultMall<${mall}>> providerResObj = null;\n            // 调用provider获取结果\n            providerResObj = ${serviceName}.get${fileName}(trace, reqMap);")
    writer.write("// 根据provider返回数据，设置响应客户端结果\n            if (providerResObj.getResultCode().equals(ThridPartyResCode._0000)) {\n                resObj.setRetCode(providerResObj.getResultData().getRetCode());\n                resObj.setRetDesc(providerResObj.getResultData().getRetMsg());\n                if (null != transObject(providerResObj)) {\n                    resObj.setRspBody(transObject(providerResObj).getRspBody());\n                }\n            } else {\n                resObj.setRetCode(ResponseCode.SERVER_FAILURE);\n                resObj.setRetDesc(providerResObj.getResultCode().getDesc());\n            }\n        }\n        return resObj;")
    writer.write("}")
    writer.write(s"private static ResBody<${mall}> transObject(ResultData<WechatMobileMallResultMall<${mall}>> providerResObj) {")
    writer.write("return null;")
    writer.write("}")
    writeNewLine(writer)
    writer.write(s"private static Map<String, String> get${fileName}Params( ReqBody<$req> reqObj){\n        Map<String, String> reqMap = new HashMap<>();\n        return reqMap;\n    }")
  }
}
