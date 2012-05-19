package br.unb.cdt.desafioPositivo.action;

import javax.faces.model.SelectItem;

import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;

import br.unb.cdt.desafioPositivo.facade.DesafioPositivoFacade;
import br.unb.cdt.desafioPositivo.model.Sexo;
import br.unb.cdt.desafioPositivo.model.Usuario;
import br.unb.cdt.desafioPositivo.util.rest.CadastroSRV;
import br.unb.cdt.desafioPositivo.util.rest.RespostaPositivo;

@Name("usuarioAction")
@AutoCreate
public class UsuarioAction {
	
	@In(required=false)
	private Usuario usuario;
	
	@In
	private DesafioPositivoFacade facade;
	

	public SelectItem[] opcoesSexo() {
		SelectItem[] items = new SelectItem[2];
		items[0] = new SelectItem();
		items[1] = new SelectItem();
		
		items[0].setValue(Sexo.MASCULINO);
		items[0].setDescription(Sexo.MASCULINO.getDescricao());
		items[0].setLabel(Sexo.MASCULINO.getDescricao());
		
		items[1].setValue(Sexo.FEMININO);
		items[1].setDescription(Sexo.FEMININO.getDescricao());
		items[1].setLabel(Sexo.FEMININO.getDescricao());
		
		return items;
	}
	
	public void cadastro() {
		try { 
			facade.adicionarUsuario(usuario);
		} 
		catch(Exception e) {
			e.printStackTrace();
		}
	}
}
