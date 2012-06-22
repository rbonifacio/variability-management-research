package br.unb.cdt.desafioPositivo.util.email;

import org.jboss.seam.ScopeType;
import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.annotations.Scope;
import org.jboss.seam.annotations.async.Asynchronous;
import org.jboss.seam.faces.Renderer;

import br.unb.cdt.desafioPositivo.facade.ExcecaoEnvioEmail;

@Name("emailUtil")
@Scope(ScopeType.CONVERSATION)
@AutoCreate
public class EmailUtil {

	@In(create=true)
	private Renderer renderer;
	@Asynchronous
	public void sendEmail(String template) throws Exception {
		try {
			renderer.render(template);
		} catch (Exception e) {
			throw new ExcecaoEnvioEmail(
					"Nao foi possivel enviar o email com a solicitacao de cadastro. Tente novamente.");
		}
	}
}
