/*******************************************************************************
 * Copyright (c) 2025 ARCAD Software.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     ARCAD Software - initial API and implementation
 *******************************************************************************/
package com.arcadsoftware.osgi;

import org.osgi.service.log.LogEntry;

/**
 * This interface define a generic logging facade which is implemented by AbstractActivator or may be used in a plain Java program and linked to any Logger interface.
 * 
 * @author ARCAD Software
 * @see AbstractActivator
 * @see AbstractLoggerFacade
 * @see JavaLogged
 * @see LoggedToConsole
 * @see LoggedToVoid
 * @see SysOutLogged
 */
public interface ILoggedPlugin {

	/**
	 * System property used to activate a TRACE level in OSGi bundles 
	 * (this trace level can be used to add debug massages in the Log).
	 * @deprecated Use the trace methods.
	 */
	public static final boolean TRACE = "true".equalsIgnoreCase(System.getProperty("com.arcadsoftware.trace")); //$NON-NLS-1$

	/**
	 * Log an simple information message.
	 *  
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 * @deprecated use #info(String)
	 */
	public void log(String message);
	
	/**
	 * Log an informal message.
	 * 
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 * @param e
	 *            the thrown exception or <code>null</code>.
	 * @deprecated use #info(String, Throwable)
	 */
	public void log(String message, Throwable e);
	
	/**
	 * Log an informal without any message.
	 * 
	 * <p>You should consider to use the <code>info(message, e)</code> version of this method
	 * to inform the user about the nature of this error.
	 * 
	 * @param e
	 *            the thrown exception or <code>null</code>.
	 * @deprecated use #info(Throwable)
	 */
	public void log(Throwable e);
	
	/**
	 * Log an simple information message.
	 *  
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 */
	public void info(String message);
	
	/**
	 * Log an informal message.
	 * 
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 * @param e
	 *            the thrown exception or <code>null</code>.
	 */
	public void info(String message, Throwable e);
	
	/**
	 * Log an informal message relative to an exception.
	 * 
	 * <p>You should consider to use the <code>info(message, e)</code> version of this method
	 * to inform the user about the nature of this error.
	 * 
	 * @param e
	 *            the thrown exception or <code>null</code>.
	 */
	public void info(Throwable e);
	
	/**
	 * Log a formatted message with the OSGi the LoggerFactory once the Logger determines the log
	 * level INFO is enabled. Use a left curly bracket (<code>'{'</code> &#92;u007B)
	 * followed by a right curly bracket (<code>'}'</code> &#92;u007D) as a place
	 * holder for an argument: <code>"{}"</code>. If you need to use the literal
	 * <code>"{}"</code> in the formatted message, precede the place holder with a
	 * reverse solidus ({@code '\'} &#92;u005C): <code>"\{}"</code>. If you need to
	 * place a backslash before the place holder, precede the reverse solidus with a
	 * reverse solidus: <code>"\\{}"</code>.
	 * <p>
	 * You can also add a {@code Throwable} and/or {@code ServiceReference} to the
	 * generated {@link LogEntry} by passing them to the logging methods as
	 * additional arguments. If the last argument is a {@code Throwable} or a
	 * {@code ServiceReference}, it is added to the generated {@link LogEntry} and
	 * then, if the next to last argument is a {@code ServiceReference} or
	 * {@code Throwable} and not the same type as the last argument, it is also
	 * added to the generated {@link LogEntry}. These arguments will not be used as
	 * message arguments. For example:
	 * 
	 * @param level The severity of the message. This should be one of the
	 *        defined log levels but may be any integer that is interpreted in a
	 *        user defined way.
	 * @param message Human readable string describing the condition or
	 *        <code>null</code>.
	 * @param arguments The arguments to format into the message.
	 */
	public void info(String message, Object... objects);

	/**
	 * Log an error message.
	 * 
	 * @param message
	 *            Human readable string describing the error or <code>null</code>.
	 * @param e
	 *            the thrown exception or <code>null</code>.
	 */
	public void error(String message, Throwable e);
	
	/**
	 * Log an error message.
	 * 
	 * @param message
	 *            Human readable string describing the error or <code>null</code>.
	 */
	public void error(String message);
	
	/**
	 * Log an error message.
	 * 
	 * @param e
	 *            the thrown exception or <code>null</code>.
	 */
	public void error(Throwable e);
	
	/**
	 * Log a formatted message with the OSGi the LoggerFactory once the Logger determines the log
	 * level ERROR is enabled. Use a left curly bracket (<code>'{'</code> &#92;u007B)
	 * followed by a right curly bracket (<code>'}'</code> &#92;u007D) as a place
	 * holder for an argument: <code>"{}"</code>. If you need to use the literal
	 * <code>"{}"</code> in the formatted message, precede the place holder with a
	 * reverse solidus ({@code '\'} &#92;u005C): <code>"\{}"</code>. If you need to
	 * place a backslash before the place holder, precede the reverse solidus with a
	 * reverse solidus: <code>"\\{}"</code>.
	 * <p>
	 * You can also add a {@code Throwable} and/or {@code ServiceReference} to the
	 * generated {@link LogEntry} by passing them to the logging methods as
	 * additional arguments. If the last argument is a {@code Throwable} or a
	 * {@code ServiceReference}, it is added to the generated {@link LogEntry} and
	 * then, if the next to last argument is a {@code ServiceReference} or
	 * {@code Throwable} and not the same type as the last argument, it is also
	 * added to the generated {@link LogEntry}. These arguments will not be used as
	 * message arguments. For example:
	 * 
	 * @param level The severity of the message. This should be one of the
	 *        defined log levels but may be any integer that is interpreted in a
	 *        user defined way.
	 * @param message Human readable string describing the condition or
	 *        <code>null</code>.
	 * @param arguments The arguments to format into the message.
	 */
	public void error(String message, Object... objects);

	/**
	 * Log a warning message.
	 * 
	 * <p>Warning errors are not critical error that do not require any user action and do
	 * not stop the current process.
	 * 
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 */
	public void warn(String message);
	
	/**
	 * Log a warning error.
	 * 
	 * <p>Warning errors are not critical error that do not require any user action and do
	 * not stop the current process.
	 * 
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 * @param e
	 *            the thrown exception or <code>null</code>.
	 */
	public void warn(String message,Throwable e);
	
	/**
	 * Log a warning error.
	 * 
	 * <p>Warning errors are not critical error that do not require any user action and do
	 * not stop the current process.
	 * 
	 * @param e
	 *            the thrown exception or <code>null</code>.
	 */
	public void warn(Throwable e);
	
	/**
	 * Log a formatted message with the OSGi the LoggerFactory once the Logger determines the log
	 * level WARN is enabled. Use a left curly bracket (<code>'{'</code> &#92;u007B)
	 * followed by a right curly bracket (<code>'}'</code> &#92;u007D) as a place
	 * holder for an argument: <code>"{}"</code>. If you need to use the literal
	 * <code>"{}"</code> in the formatted message, precede the place holder with a
	 * reverse solidus ({@code '\'} &#92;u005C): <code>"\{}"</code>. If you need to
	 * place a backslash before the place holder, precede the reverse solidus with a
	 * reverse solidus: <code>"\\{}"</code>.
	 * <p>
	 * You can also add a {@code Throwable} and/or {@code ServiceReference} to the
	 * generated {@link LogEntry} by passing them to the logging methods as
	 * additional arguments. If the last argument is a {@code Throwable} or a
	 * {@code ServiceReference}, it is added to the generated {@link LogEntry} and
	 * then, if the next to last argument is a {@code ServiceReference} or
	 * {@code Throwable} and not the same type as the last argument, it is also
	 * added to the generated {@link LogEntry}. These arguments will not be used as
	 * message arguments. For example:
	 * 
	 * @param level The severity of the message. This should be one of the
	 *        defined log levels but may be any integer that is interpreted in a
	 *        user defined way.
	 * @param message Human readable string describing the condition or
	 *        <code>null</code>.
	 * @param arguments The arguments to format into the message.
	 */
	public void warn(String message, Object... objects);
	
	/**
	 * If the plugin is in debug mode then log the message.
	 * 
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 */
	public void debug(String message);
	
	/**
	 * If the plugin is in debug mode then log the exception
	 * 
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 * @param e
	 *            the thrown exception or <code>null</code>.
	 */
	public void debug(String message, Throwable e);

	/**
	 * If the plugin is in debug mode then log the exception
	 * 
	 * @param e
	 *            the thrown exception or <code>null</code>.
	 */
	public void debug(Throwable e);
	
	/**
	 * Log at the Debug level a formatted message with the OSGi the LoggerFactory once the Logger determines the log
	 * level is enabled. Use a left curly bracket (<code>'{'</code> &#92;u007B)
	 * followed by a right curly bracket (<code>'}'</code> &#92;u007D) as a place
	 * holder for an argument: <code>"{}"</code>. If you need to use the literal
	 * <code>"{}"</code> in the formatted message, precede the place holder with a
	 * reverse solidus ({@code '\'} &#92;u005C): <code>"\{}"</code>. If you need to
	 * place a backslash before the place holder, precede the reverse solidus with a
	 * reverse solidus: <code>"\\{}"</code>.
	 * <p>
	 * You can also add a {@code Throwable} and/or {@code ServiceReference} to the
	 * generated {@link LogEntry} by passing them to the logging methods as
	 * additional arguments. If the last argument is a {@code Throwable} or a
	 * {@code ServiceReference}, it is added to the generated {@link LogEntry} and
	 * then, if the next to last argument is a {@code ServiceReference} or
	 * {@code Throwable} and not the same type as the last argument, it is also
	 * added to the generated {@link LogEntry}. These arguments will not be used as
	 * message arguments. For example:
	 * 
	 * @param level The severity of the message. This should be one of the
	 *        defined log levels but may be any integer that is interpreted in a
	 *        user defined way.
	 * @param message Human readable string describing the condition or
	 *        <code>null</code>.
	 * @param arguments The arguments to format into the message.
	 */
	public void debug(String message, Object... objects);
	
	/**
	 * Logs a debug message.
	 * 
	 * @param message
	 *            Human readable message or <code>null</code>.
	 */
	public void trace(String message);
	
	/**
	 * Logs a debug message.
	 * 
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 * @param e
	 *            the thrown exception or <code>null</code>.
	 */
	public void trace(String message, Throwable e);
	
	/**
	 * Log an Exception to the debug level.
	 * 
	 * @param e the exception, may be null. 
	 */
	public void trace(Throwable e);
	
	/**
	 * Trace a formatted message with the OSGi the LoggerFactory once the Logger determines the log
	 * level is enabled. Use a left curly bracket (<code>'{'</code> &#92;u007B)
	 * followed by a right curly bracket (<code>'}'</code> &#92;u007D) as a place
	 * holder for an argument: <code>"{}"</code>. If you need to use the literal
	 * <code>"{}"</code> in the formatted message, precede the place holder with a
	 * reverse solidus ({@code '\'} &#92;u005C): <code>"\{}"</code>. If you need to
	 * place a backslash before the place holder, precede the reverse solidus with a
	 * reverse solidus: <code>"\\{}"</code>.
	 * <p>
	 * You can also add a {@code Throwable} and/or {@code ServiceReference} to the
	 * generated {@link LogEntry} by passing them to the logging methods as
	 * additional arguments. If the last argument is a {@code Throwable} or a
	 * {@code ServiceReference}, it is added to the generated {@link LogEntry} and
	 * then, if the next to last argument is a {@code ServiceReference} or
	 * {@code Throwable} and not the same type as the last argument, it is also
	 * added to the generated {@link LogEntry}. These arguments will not be used as
	 * message arguments. For example:
	 * 
	 * @param level The severity of the message. This should be one of the
	 *        defined log levels but may be any integer that is interpreted in a
	 *        user defined way.
	 * @param message Human readable string describing the condition or
	 *        <code>null</code>.
	 * @param arguments The arguments to format into the message.
	 */
	public void trace(String message, Object... objects);
	
	/**
	 * Logs a message which will be logged regardless of active level.
	 * 
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 * @param e
	 *            The exception that reflects the condition or <code>null</code>.
	 */
	public void audit(String message, Throwable e);
	
	/**
	 * Logs a message which will be logged regardless of active level.
	 * 
	 * @param message
	 *            Human readable string describing the condition or <code>null</code>.
	 */
	public void audit(String message);

	/**
	 * Logs a message which will be logged regardless of active level.
	 * 
	 * @param e
	 *            The exception that reflects the condition or <code>null</code>.
	 */
	public void audit(Throwable e);
	
	/**
	 * Log a formatted message with the OSGi the LoggerFactory et to top most level.
	 * Use a left curly bracket (<code>'{'</code> &#92;u007B)
	 * followed by a right curly bracket (<code>'}'</code> &#92;u007D) as a place
	 * holder for an argument: <code>"{}"</code>. If you need to use the literal
	 * <code>"{}"</code> in the formatted message, precede the place holder with a
	 * reverse solidus ({@code '\'} &#92;u005C): <code>"\{}"</code>. If you need to
	 * place a backslash before the place holder, precede the reverse solidus with a
	 * reverse solidus: <code>"\\{}"</code>.
	 * <p>
	 * You can also add a {@code Throwable} and/or {@code ServiceReference} to the
	 * generated {@link LogEntry} by passing them to the logging methods as
	 * additional arguments. If the last argument is a {@code Throwable} or a
	 * {@code ServiceReference}, it is added to the generated {@link LogEntry} and
	 * then, if the next to last argument is a {@code ServiceReference} or
	 * {@code Throwable} and not the same type as the last argument, it is also
	 * added to the generated {@link LogEntry}. These arguments will not be used as
	 * message arguments. For example:
	 * 
	 * @param level The severity of the message. This should be one of the
	 *        defined log levels but may be any integer that is interpreted in a
	 *        user defined way.
	 * @param message Human readable string describing the condition or
	 *        <code>null</code>.
	 * @param arguments The arguments to format into the message.
	 */
	public void audit(String message, Object... objects);

}
