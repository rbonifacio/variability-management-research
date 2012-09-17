package br.unb.cdt.desafioPositivo.util.email;

import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.mail.Message;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import br.unb.cdt.desafioPositivo.facade.ExcecaoEnvioEmail;
/**
 * Componente SEAM utilizado para o envio de emails.
 * 
 * A configuracao desse componente eh feita com o 
 * uso do arquivo components.xml. Necessario ajustar 
 * nos momentos de deployment e producao.
 * 
 * @author rbonifacio
 */
public class EmailUtil {
	
	private static final String EMAIL_PATTERN =  "^[_A-Za-z0-9-]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,})$";

	
	/* configuracao padrao do componente */
	
	/*
	private boolean desenvolvimento = false;
	private String autorizacao = "false";
	private String tls = "false";
	private String host = "localhost";
	private String porta = "25";
	private String usuario = "desafiopositivo@concursoideiapp.com.br";
	private String senha = "";
	private String[] listaInternaDestinatarios = new String[] { "desafiopositivoandroid@gmail.com" };
	*/
	private boolean desenvolvimento = true;
	private String autorizacao = "true";
	private String tls = "true";
	private String host = "smtp.gmail.com";
	private String porta = "587";
	private String usuario = "desafiopositivoandroid@gmail.com";
	private String senha = "android@0!2";
	private String[] listaInternaDestinatarios = new String[] { "desafiopositivoandroid@gmail.com", "eusyar@gmail.com" };
	
	/**
	 * Realiza o envio do email. 
	 * @param destino - destinatario da mensagem
	 * @param assunto - assunto da mensagem 
	 * @param corpo - corpo da mensagem
	 * @throws Exception
	 */
	public void enviarEmail(String[] listaDestinatarios, String assunto, String corpo) throws Exception {
		try {	
			Properties props = new Properties();
			props.put("mail.smtp.auth", autorizacao);
			props.put("mail.smtp.starttls.enable", tls);
			props.put("mail.smtp.host", host);
			props.put("mail.smtp.port", porta);
				
			Session session = Session.getInstance(props,
					  new javax.mail.Authenticator() {
						protected PasswordAuthentication getPasswordAuthentication() {
							return new PasswordAuthentication(usuario, senha);
						}
					  });
			
			session.setDebug(desenvolvimento);
			
			MimeMessage message = new MimeMessage(session);
			
			message.setFrom(new InternetAddress(usuario));
			
			populaListaDestinatarios(message, desenvolvimento ? listaInternaDestinatarios : listaDestinatarios);
			
			message.setSubject(assunto);
			message.setText(corpo);
			
			Transport.send(message);	  
		} catch (Exception e) {
			e.printStackTrace();
			throw new ExcecaoEnvioEmail(
					"Nao foi possivel enviar o email com a solicitacao de cadastro. Tente novamente.");
		}
	}
	
	public void enviarEmailContato(String[] listaDestinatarios, String assunto, String corpo) throws Exception {
		try {	
			Properties props = new Properties();
			props.put("mail.smtp.auth", autorizacao);
			props.put("mail.smtp.starttls.enable", tls);
			props.put("mail.smtp.host", host);
			props.put("mail.smtp.port", porta);
				
			Session session = Session.getInstance(props,
					  new javax.mail.Authenticator() {
						protected PasswordAuthentication getPasswordAuthentication() {
							return new PasswordAuthentication(usuario, senha);
						}
					  });
			
			session.setDebug(desenvolvimento);
			
			MimeMessage message = new MimeMessage(session);
			
			message.setFrom(new InternetAddress(usuario));
			
			populaListaDestinatarios(message, new String[] { "desafiopositivo@cdt.unb.br" });
			
			message.setSubject(assunto);
			message.setText(corpo);
			
			Transport.send(message);	  
		} catch (Exception e) {
			e.printStackTrace();
			throw new ExcecaoEnvioEmail(
					"Nao foi possivel enviar o email comR o contato. Tente novamente.");
		}
	}
	
	public void enviarEmailLog(String fluxo, String corpo) {
		try {	
			Properties props = new Properties();
			props.put("mail.smtp.auth", autorizacao);
			props.put("mail.smtp.starttls.enable", tls);
			props.put("mail.smtp.host", host);
			props.put("mail.smtp.port", porta);
				
			Session session = Session.getInstance(props,
					  new javax.mail.Authenticator() {
						protected PasswordAuthentication getPasswordAuthentication() {
							return new PasswordAuthentication(usuario, senha);
						}
					  });
			
			session.setDebug(desenvolvimento);
			
			MimeMessage message = new MimeMessage(session);
			
			message.setFrom(new InternetAddress(usuario));
			
			populaListaDestinatarios(message, new String[] { "desafiopositivoandroidlog@gmail.com" });
			
			message.setSubject(fluxo);
			message.setText(corpo);
			
			Transport.send(message);	  
		} catch (Exception e) {
			e.printStackTrace();
			System.out.println("Erro ao enviar Log.");
		}
	}
	

	private void populaListaDestinatarios(MimeMessage message, String[] listaDestinatarios) throws Exception{
		for(String destinatario: listaDestinatarios) {
			message.addRecipient(Message.RecipientType.TO, new InternetAddress(destinatario));
		}
	}
	public boolean isDesenvolvimento() {
		return desenvolvimento;
	}

	public void setDesenvolvimento(boolean desenvolvimento) {
		this.desenvolvimento = desenvolvimento;
	}

	public boolean verificaEmailValido(String email) {
		Pattern pattern = Pattern.compile(EMAIL_PATTERN);
		Matcher matcher = pattern.matcher(email);
		return matcher.matches();
	}

	
	public String getAutorizacao() {
		return autorizacao;
	}

	public void setAutorizacao(String autorizacao) {
		this.autorizacao = autorizacao;
	}

	public String getTls() {
		return tls;
	}

	public void setTls(String tls) {
		this.tls = tls;
	}

	public String getHost() {
		return host;
	}

	public void setHost(String host) {
		this.host = host;
	}

	public String getPorta() {
		return porta;
	}

	public void setPorta(String porta) {
		this.porta = porta;
	}

	public String getUsuario() {
		return usuario;
	}

	public void setUsuario(String usuario) {
		this.usuario = usuario;
	}

	public String getSenha() {
		return senha;
	}

	public void setSenha(String senha) {
		this.senha = senha;
	}

	public String[] getListaInternaDestinatarios() {
		return listaInternaDestinatarios;
	}

	public void setListaInternaDestinatarios(String[] listaInternaDestinatarios) {
		this.listaInternaDestinatarios = listaInternaDestinatarios;
	}
}
