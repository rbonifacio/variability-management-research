package br.unb.cdt.desafioPositivo.util.email;

import java.util.Properties;

import javax.mail.Message;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import org.jboss.seam.ScopeType;
import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.annotations.Scope;

import br.unb.cdt.desafioPositivo.facade.ExcecaoEnvioEmail;

@Name("emailUtil")
@Scope(ScopeType.CONVERSATION)
@AutoCreate
public class EmailUtil {

//	@In(create=true)
//	private Renderer renderer;
	
	public void sendEmail(String from, String to, String subject, String body) throws Exception {
		try {
			//renderer.render(template);
			
			Properties properties = System.getProperties();
			
			properties.setProperty("mail.smtp.host", "localhost");
			properties.setProperty("mail.smtp.port", "25");
			
			Session session = Session.getDefaultInstance(properties);
			
			MimeMessage message = new MimeMessage(session);
			
			message.setFrom(new InternetAddress(from));
			message.addRecipient(Message.RecipientType.TO, new InternetAddress(to));
			message.setSubject(subject);
			message.setText(body);
			
			Transport.send(message);
			
			System.out.println("[EmailUtil] message sent");
		} catch (Exception e) {
			throw new ExcecaoEnvioEmail(
					"Nao foi possivel enviar o email com a solicitacao de cadastro. Tente novamente.");
		}
	}
}
