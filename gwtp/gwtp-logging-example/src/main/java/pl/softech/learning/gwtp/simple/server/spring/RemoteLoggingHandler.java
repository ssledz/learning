/*
 * Copyright 2013 Sławomir Śledź <slawomir.sledz@sof-tech.pl>.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package pl.softech.learning.gwtp.simple.server.spring;

import java.io.IOException;
import java.net.URL;
import java.util.logging.LogRecord;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.web.HttpRequestHandler;
import org.springframework.web.context.ServletContextAware;

import com.google.gwt.logging.server.StackTraceDeobfuscator;
import com.google.gwt.logging.shared.RemoteLoggingService;
import com.google.gwt.user.server.rpc.RemoteServiceServlet;

/**
 * @author Sławomir Śledź <slawomir.sledz@sof-tech.pl>
 * @since 1.0
 */
@Component("remoteLogging")
public class RemoteLoggingHandler extends RemoteServiceServlet implements HttpRequestHandler, ServletContextAware,
		RemoteLoggingService {

	private static final long serialVersionUID = 1L;

	private static final Logger LOGGER = LoggerFactory.getLogger(RemoteLoggingHandler.class);

	private static StackTraceDeobfuscator deobfuscator = null;

	private ServletContext servletContext;

	@Override
	public void handleRequest(final HttpServletRequest request, final HttpServletResponse response)
			throws ServletException, IOException {
		doPost(request, response);
	}

	@Override
	public String logOnServer(LogRecord lr) {
		final String strongName = getPermutationStrongName();

		if (deobfuscator != null) {
			lr = deobfuscator.deobfuscateLogRecord(lr, strongName);
		}

		try {
			final Logger logger = LoggerFactory.getLogger(lr.getLoggerName());
			LogLevel.level(lr.getLevel()).log(logger, lr);
		} catch (final Exception e) {
			LOGGER.error("Remote logging failed", e);
			return "Remote logging failed, check stack trace for details.";
		}

		return null;
	}

	@Override
	public void setServletContext(final ServletContext servletContext) {
		this.servletContext = servletContext;

		final String path = servletContext.getInitParameter("symbolMapsPath");
		if (path != null) {
			URL url = RemoteLoggingHandler.class.getResource("/");
			LOGGER.debug("Current path is {}", url.getFile());
			String rootPath = "/../../";
			URL rootPathUrl = RemoteLoggingHandler.class.getResource(rootPath);
			if (rootPathUrl != null) {
				LOGGER.debug("Root app path is {}", rootPathUrl.getFile());
			}
			URL symbolMapsUrl = RemoteLoggingHandler.class.getResource(String.format("%s%s", rootPath, path));
			if (symbolMapsUrl != null) {
				String symbolMapsDir = symbolMapsUrl.getPath();
				LOGGER.debug("SymbolMaps absolute path is {}", symbolMapsDir);
				deobfuscator = new StackTraceDeobfuscator(symbolMapsDir);
				LOGGER.debug("Created deobfuscator with symbolMaps located {}", path);
			}
		}
	}

	@Override
	public ServletContext getServletContext() {
		return servletContext;
	}

}
