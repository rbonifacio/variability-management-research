package br.unb.cdt.desafioPositivo.action;

import org.hibernate.validator.Email;
import org.hibernate.validator.NotNull;
import org.hibernate.validator.Pattern;
import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.international.StatusMessage;
import org.jboss.seam.international.StatusMessages;

import br.unb.cdt.desafioPositivo.facade.DesafioPositivoFacade;
import br.unb.cdt.desafioPositivo.facade.ExcecaoEnvioEmail;
import br.unb.cdt.desafioPositivo.facade.ExcecaoNomeInvalido;
import br.unb.cdt.desafioPositivo.mensagens.Mensagens;
import br.unb.cdt.desafioPositivo.model.Usuario;
import br.unb.cdt.desafioPositivo.util.email.EmailUtil;

@Name("contatoAction")
@AutoCreate
public class ContatoAction {

	@Pattern(regex="[\\p{L}\\p{Space}]+", message="O nome deve conter apenas letras e espaços em branco")
	@NotNull(message="A mensagem não pode ser anónima")
	private String nome;
	
	@Pattern(regex="^[_A-Za-z0-9-]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,})$", message="O e-mail inserido não é válido")
	private String email;

	private String assunto;
	private String mensagem;
	
	@In(create=true)
	private EmailUtil emailUtil;
	
	public String enviarContato(){
		
		if(!validarEmailContato() ){
			return "home";
		}
		
		if(!validarNome()) {
			StatusMessages.instance().addFromResourceBundle(
					StatusMessage.Severity.ERROR, Mensagens.NOME_INVALIDO);
			return null;
		}
			
		String pre_ass = "Contato, de " + nome + " : ";
		String cabeca = "Autor: " + nome + "\nE-mail: "+ email +" \n";
		
		try{
			emailUtil.enviarEmailContato(new String[] {email}, pre_ass + assunto, cabeca+ mensagem);
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, 
					"O seu e-mail de contato foi enviado com sucesso!\n Aguarde a resposta.");
			return "home";
		} catch (Exception e) {
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, 
					"Ocorreu um erro ao enviar a mensagem.\nTente novamente mais tarde.");
			return null;
		}

	}
	
	
	private boolean validarEmailContato() {
		// TODO Auto-generated method stub
		//if()
		return true;
	}
	
	private boolean validarNome() {
		if(nome == null) {
			return true;
		}
		
		if (nome.contains("!") || 
				nome.contains("@") || 
				nome.contains("#") ||
				nome.contains("$") || 
				nome.contains("%") || 
				nome.contains("�") || 
				nome.contains("&") || 
				nome.contains("*") || 
				nome.contains("(") || 
				nome.contains(")") || 
				nome.contains("-") || 
				nome.contains("_") || 
				nome.contains("+") || 
				nome.contains("=") || 
				nome.contains("�") || 
				nome.contains("[") || 
				nome.contains("{") || 
				nome.contains("]") || 
				nome.contains("}") || 
				nome.contains(";") || 
				nome.contains(":") || 
				nome.contains(".") || 
				nome.contains(",") || 
				nome.contains(">") || 
				nome.contains("<") || 
				nome.contains("0") ||
				nome.contains("1") ||
				nome.contains("2") ||
				nome.contains("3") ||
				nome.contains("4") ||
				nome.contains("5") ||
				nome.contains("6") ||
				nome.contains("7") ||
				nome.contains("8") ||
				nome.contains("9")) {
			return false;
		}
		return true;
	}


	public String getNome() {
		return nome;
	}

	public void setNome(String nome) {
		this.nome = nome;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getAssunto() {
		return assunto;
	}

	public void setAssunto(String assunto) {
		this.assunto = assunto;
	}

	public String getMensagem() {
		return mensagem;
	}

	public void setMensagem(String mensagem) {
		this.mensagem = mensagem;
	}
	
}
