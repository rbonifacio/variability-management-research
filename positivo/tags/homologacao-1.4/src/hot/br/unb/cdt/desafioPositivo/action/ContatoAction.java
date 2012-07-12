package br.unb.cdt.desafioPositivo.action;

import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.international.StatusMessage;
import org.jboss.seam.international.StatusMessages;

import br.unb.cdt.desafioPositivo.facade.ExcecaoEnvioEmail;
import br.unb.cdt.desafioPositivo.mensagens.Mensagens;
import br.unb.cdt.desafioPositivo.util.email.EmailUtil;

@Name("contatoAction")
@AutoCreate
public class ContatoAction {

	private String nome;
	private String email;
	private String assunto;
	private String mensagem;
	
	@In(create=true)
	private EmailUtil emailUtil;
	
	public String enviarContato(){
		
		if(! validarEmailContato() ){
			return "home";
		}
			
		String pre_ass = "Contato, de " + nome + " : ";
		String cabeca = "Autor: " + nome + "\n\n";
		
		try{
			emailUtil.enviarEmailContato(new String[] {email}, pre_ass + assunto, cabeca+ mensagem);
		} catch (Exception e) {
			
		}
		StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, 
				"O seu e-mail de contato foi enviado com sucesso!\n Aguarde a resposta");
		return "";
	}
	
	
	private boolean validarEmailContato() {
		// TODO Auto-generated method stub
		//if()
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
