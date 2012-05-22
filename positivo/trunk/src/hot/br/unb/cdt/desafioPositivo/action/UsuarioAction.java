package br.unb.cdt.desafioPositivo.action;

import javax.faces.application.FacesMessage;
import javax.faces.model.SelectItem;

import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.faces.FacesMessages;
import org.jboss.seam.security.Identity;

import br.unb.cdt.desafioPositivo.facade.DesafioPositivoFacade;
import br.unb.cdt.desafioPositivo.facade.ExcecaoUsuarioCadastrado;
import br.unb.cdt.desafioPositivo.model.Estado;
import br.unb.cdt.desafioPositivo.model.Sexo;
import br.unb.cdt.desafioPositivo.model.Usuario;

@Name("usuarioAction")
@AutoCreate
public class UsuarioAction {
	
	@In(required=false)
	private Usuario usuario;
	
	@In
	private DesafioPositivoFacade facade;
	
	@In 
	private FacesMessages facesMessages;
	
	@In
	private Identity identity;

	public SelectItem[] opcoesEstado() {
		SelectItem[] items = new SelectItem[Estado.values().length];
		
		int i = 0;
		
		for(Estado e : Estado.values()) {
			items[i] = new SelectItem();
			
			items[i].setValue(e);
			items[i].setLabel(e.getSigla());
			items[i].setDescription(e.getEstado());
			
			i++;
		}
		
		return items;
	}
	
	public SelectItem[] opcoesSexo() {
		SelectItem[] items = new SelectItem[Sexo.values().length];
		
		int i = 0;
		for(Sexo s : Sexo.values()) {
			items[i] = new SelectItem();
		
			items[i].setValue(s);
			items[i].setLabel(s.getDescricao());
			items[i].setDescription(s.getDescricao());
			
			i++;
		}
		
		return items;
	}
	
	public String cadastro() {
		try { 
			facade.adicionarUsuario(usuario);
			facesMessages.add(FacesMessage.SEVERITY_INFO, "Usuario cadastrado com sucesso. Proceda com a autenticacao");
			return "home";
		} 
		catch(ExcecaoUsuarioCadastrado e) {
			facesMessages.add(FacesMessage.SEVERITY_ERROR, "Jah existe um usuario com o email " + usuario.getEmail() + " cadastrado na rede Positivo");
			return null;
		}
		catch(Exception e) {
			e.printStackTrace();
			facesMessages.add(FacesMessage.SEVERITY_ERROR, e.getLocalizedMessage());
			return null;
		}
	}
	
	public String autenticar() {
		if(identity.login().equals("loggedIn")) {
			return "sumario";
		}
		return "home";
	}
}
