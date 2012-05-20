package br.unb.cdt.desafioPositivo.action;

import javax.faces.application.FacesMessage;
import javax.faces.model.SelectItem;

import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.faces.FacesMessages;

import br.unb.cdt.desafioPositivo.facade.DesafioPositivoFacade;
import br.unb.cdt.desafioPositivo.facade.ExcecaoUsuarioCadastrado;
import br.unb.cdt.desafioPositivo.model.Sexo;
import br.unb.cdt.desafioPositivo.model.Usuario;

@Name("usuarioAction")
@AutoCreate
public class UsuarioAction {
	
	@In(required=false)
	private Usuario usuario;
	
	@In
	private DesafioPositivoFacade facade;
	
	@In FacesMessages facesMessages;
	

	public SelectItem[] opcoesSexo() {
		SelectItem[] items = new SelectItem[2];
		items[0] = new SelectItem();
		items[1] = new SelectItem();
		
		items[0].setValue(Sexo.MASCULINO);
		items[0].setDescription(Sexo.MASCULINO.getDescricao());
		items[0].setLabel("M");
		
		items[1].setValue(Sexo.FEMININO);
		items[1].setDescription(Sexo.FEMININO.getDescricao());
		items[1].setLabel("F");
		
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
}
