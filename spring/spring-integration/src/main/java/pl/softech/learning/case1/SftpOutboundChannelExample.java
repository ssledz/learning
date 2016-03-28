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
package pl.softech.learning.case1;

import java.nio.charset.Charset;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageChannel;
import org.springframework.messaging.support.MessageBuilder;

/**
 * @author Sławomir Śledź <slawomir.sledz@sof-tech.pl>
 * @since 1.0
 */
public class SftpOutboundChannelExample {

	public static void main(final String[] args) {
		final ApplicationContext ctx = new ClassPathXmlApplicationContext("pl/softech/learning/case1/context.xml");
		sendFileUsingChannel(ctx);
		sendFileUsingGateway(ctx);
		sendAndRenameFile(ctx);
	}

	private static void sendAndRenameFile(final ApplicationContext ctx) {
		final FileUploadGateway uploadGateway = ctx.getBean("sftpFileUploadGateway", FileUploadGateway.class);
		final RenameFileGateway renameGateway = ctx.getBean("sftpRenameFileGateway", RenameFileGateway.class);
		
		uploadGateway.upload("Sample content \n".getBytes(Charset.forName("UTF-8")), "rename-file.txt");
		renameGateway.rename("rename-file.txt", "rename-file.txt.done");
	}
	
	private static void sendFileUsingGateway(final ApplicationContext ctx) {
		final FileUploadGateway uploadGateway = ctx.getBean("sftpFileUploadGateway", FileUploadGateway.class);
		uploadGateway.upload("Sample content \n".getBytes(Charset.forName("UTF-8")), "using-gateway.txt");
	}
	
	private static void sendFileUsingChannel(final ApplicationContext ctx) {
		final MessageChannel channel = ctx.getBean("outputFilesChannel", MessageChannel.class);
		final byte[] bytes = "Sample content \n".getBytes(Charset.forName("UTF-8"));
		final Message<byte[]> payload = MessageBuilder.withPayload(bytes)//
				.setHeader("outputFileName", "using-channel.txt").build();
		
		channel.send(payload);
		
	}}
