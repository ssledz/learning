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

import java.util.Map;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import org.slf4j.Logger;

import com.google.common.collect.Maps;

/**
 * 
 * @author Sławomir Śledź <slawomir.sledz@sof-tech.pl>
 * @since 1.0
 */
public enum LogLevel {

	OFF(Level.OFF) {

		@Override
		public void log(final Logger logger, final String msg) {
		}

		@Override
		public void log(final Logger logger, final String format, final Object[] argArray) {
		}

		@Override
		public void log(final Logger logger, final String msg, final Throwable throwable) {
		}

		@Override
		public boolean isEnabledFor(final Logger logger) {
			return false;
		}

	},

	ERROR(Level.SEVERE) {
		@Override
		public void log(final Logger logger, final String msg) {
			logger.error(msg);
		}

		@Override
		public void log(final Logger logger, final String format, final Object[] argArray) {
			logger.error(format, argArray);
		}

		@Override
		public void log(final Logger logger, final String msg, final Throwable throwable) {
			logger.error(msg, throwable);
		}

		@Override
		public boolean isEnabledFor(final Logger logger) {
			return logger.isErrorEnabled();
		}
	},

	WARN(Level.WARNING) {
		@Override
		public void log(final Logger logger, final String msg) {
			logger.warn(msg);
		}

		@Override
		public void log(final Logger logger, final String format, final Object[] argArray) {
			logger.warn(format, argArray);
		}

		@Override
		public void log(final Logger logger, final String msg, final Throwable throwable) {
			logger.warn(msg, throwable);
		}

		@Override
		public boolean isEnabledFor(final Logger logger) {
			return logger.isWarnEnabled();
		}

	},

	INFO(Level.INFO, Level.CONFIG) {
		@Override
		public void log(final Logger logger, final String msg) {
			logger.info(msg);
		}

		@Override
		public void log(final Logger logger, final String format, final Object[] argArray) {
			logger.info(format, argArray);
		}

		@Override
		public void log(final Logger logger, final String msg, final Throwable throwable) {
			logger.info(msg, throwable);
		}

		@Override
		public boolean isEnabledFor(final Logger logger) {
			return logger.isInfoEnabled();
		}
	},

	DEBUG(Level.FINE) {
		@Override
		public void log(final Logger logger, final String msg) {
			logger.debug(msg);
		}

		@Override
		public void log(final Logger logger, final String format, final Object[] argArray) {
			logger.debug(format, argArray);
		}

		@Override
		public void log(final Logger logger, final String msg, final Throwable throwable) {
			logger.debug(msg, throwable);
		}

		@Override
		public boolean isEnabledFor(final Logger logger) {
			return logger.isDebugEnabled();
		}
	},

	TRACE(Level.FINER, Level.FINEST, Level.ALL) {
		@Override
		public void log(final Logger logger, final String msg) {
			logger.trace(msg);
		}

		@Override
		public void log(final Logger logger, final String format, final Object[] argArray) {
			logger.trace(format, argArray);
		}

		@Override
		public void log(final Logger logger, final String msg, final Throwable throwable) {
			logger.trace(msg, throwable);
		}

		@Override
		public boolean isEnabledFor(final Logger logger) {
			return logger.isTraceEnabled();
		}
	};

	private static final Map<Level, LogLevel> MAP = Maps.newHashMap();

	static {
		for (final LogLevel ll : LogLevel.values()) {
			for (final Level l : ll.level) {
				MAP.put(l, ll);
			}
		}
	}

	private Level[] level;

	private LogLevel(final Level... level) {
		this.level = level;
	}

	public static LogLevel level(final Level l) {
		LogLevel ll = MAP.get(l);
		if (ll == null) {
			ll = LogLevel.OFF;
		}
		return ll;
	}

	public abstract void log(final Logger logger, final String msg);

	public abstract void log(final Logger logger, final String format, final Object[] argArray);

	public abstract void log(final Logger logger, final String msg, final Throwable throwable);

	public abstract boolean isEnabledFor(final Logger logger);

	public void log(final Logger logger, final LogRecord lr) {

		if (lr.getThrown() != null) {
			log(logger, lr.getMessage(), lr.getThrown());
			return;
		}
		
		if(lr.getParameters() != null) {
			log(logger, lr.getMessage(), lr.getParameters());
			return;
		}
		
		log(logger, lr.getMessage());

	}

}
